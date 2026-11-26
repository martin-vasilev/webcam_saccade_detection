"""Independent joint selection and saved-event scoring. No detector/model calls."""
from pathlib import Path
from collections import defaultdict
import hashlib
import json
import sys
sys.dont_write_bytecode = True
import numpy as np
import pandas as pd

root, output = map(Path, sys.argv[1:3])
registry = pd.read_csv(root / 'config/part2/input_manifest.csv').fillna('')
paths = dict(zip(registry.id, registry.path))
def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(4194304), b''): h.update(chunk)
    return h.hexdigest()
for row in registry.itertuples(): assert sha(row.path) == row.sha256, row.id
def read(key): return pd.read_csv(paths[key], float_precision='round_trip')
def table(name): return pd.read_csv(output / 'tables' / (name + '.csv'), float_precision='round_trip')
def metric(x):
    tp, tn, fp, fn = map(float, x)
    denominator = (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
    divide = lambda a,b: a/b if b else np.nan
    return np.array([divide(tp*tn-fp*fn, np.sqrt(denominator)), divide(2*tp,2*tp+fp+fn),divide(tp,tp+fp),divide(tp,tp+fn)])
def close(a,b): assert np.allclose(a,b,atol=1e-12,rtol=0,equal_nan=True), (a,b)
def text_indices(indices): return ','.join(map(str,indices))
def digest_ids(indices): return hashlib.sha256(text_indices(indices).encode()).hexdigest()
trials = read('trials'); mask = read('mask'); mask = mask[mask.common_eligible].copy()
keys = list(zip(trials['sub'], trials.Trial_Id)); key_index = {key:i for i,key in enumerate(keys)}
row_trials = np.array([key_index[k] for k in zip(mask['sub'],mask.Trial_Id)])
weights = np.bincount(row_trials,minlength=792)
assert len(mask)==68577 and len(keys)==len(key_index)==792 and mask.source_row_id.is_unique
participants = sorted(trials['sub'].unique()); assert len(participants)==20
definitions = read('b_grid')
hd = read('h_grid'); hd['threshold_value']=np.nan
hd['preprocessing']=hd.smoothing.map({'raw':'Raw','mean':'Mean3','median':'Median3','sg':'SG5_23'})
defs = pd.concat([definitions,hd],ignore_index=True).set_index('configuration_id')
cached = {'b': read('b_counts'), 'h': read('h_counts')}
calls = {}
for condition in ['original','rate30']:
    states = {}
    for family in ['b','h']:
        st = read(f'{family}_status_{condition}')
        assert not st.duplicated(['configuration_id','sub','Trial_Id']).any()
        states.update({(r.configuration_id,r.sub,r.Trial_Id):r.status for r in st.itertuples()})
        frame = cached[family]; frame = frame[frame.condition==condition]
        for cfg,g in frame.groupby('configuration_id',sort=False):
            g = g.set_index(['sub','Trial_Id']).loc[keys]
            status = g.status.to_numpy(); ok = np.isin(status,['success','success_empty'])
            assert len(g)==792
            assert all(states[(cfg,*key)]==state for key,state in zip(keys,status))
            assert np.array_equal(g.common_rows,weights)
            counts = g[['TP','TN','FP','FN']].to_numpy()
            assert np.array_equal(counts[ok].sum(axis=1),weights[ok])
            event = defs.loc[cfg,'event']; positive = np.bincount(row_trials[mask.ground_truth.eq(event)],minlength=792)
            assert np.array_equal(counts[ok,0]+counts[ok,3],positive[ok])
            counts[~ok]=0
            calls[condition,cfg]=(ok,counts,status)
    assert len(states)==328*792
print('All cached candidate supports/statuses independently aligned.',flush=True)
selection = table('selected_configurations'); candidates = table('candidate_scores'); trainlog = table('training_support')
assert len(selection)==160 and (selection.outcome=='selected').all()
verified_candidates=0
for condition in ['original','rate30']:
    for method in ['IVT','IDT','EK03','IHMM']:
        pairs=[]
        for pre in ['Raw','Mean3','Median3','SG5_23']:
            g=defs[(defs.method==method)&(defs.preprocessing==pre)]
            fixes=g[g.event=='fixation'].sort_values('threshold_value').index
            saccades=g[g.event=='saccade'].sort_values('threshold_value').index
            pairs.extend((pre,f,s) for f in fixes for s in saccades)
        available=np.array([calls[condition,f][0]&calls[condition,s][0] for _,f,s in pairs])
        for sub in participants:
            train=trials['sub'].to_numpy()!=sub
            totals=available@(weights*train)
            maximum=totals.max(); tied=np.flatnonzero(totals==maximum)
            aligned=train&available[tied].all(axis=0)
            assert not aligned[~train].any()
            score_by_cfg={cfg:metric(calls[condition,cfg][1][aligned].sum(axis=0))[0] for i in tied for cfg in pairs[i][1:]}
            scores={i:(score_by_cfg[pairs[i][1]]+score_by_cfg[pairs[i][2]])/2 for i in tied}
            admissible=[i for i in tied if np.isfinite(scores[i])]
            assert admissible and maximum>0 and (weights*aligned).sum()>0
            best=max(scores[i] for i in admissible);winner=next(i for i in admissible if scores[i]>=best-1e-12)
            expected=pairs[winner]
            saved=selection[(selection.heldout_sub==sub)&(selection.condition==condition)&(selection.method==method)].iloc[0]
            assert (saved.preprocessing,saved.fix_id,saved.sacc_id)==expected
            close(saved.training_J,scores[winner])
            ids=np.sort(mask.source_row_id.to_numpy()[aligned[row_trials]])
            assert saved.aligned_training_row_sha256==digest_ids(ids)
            logs=trainlog[(trainlog.heldout_sub==sub)&(trainlog.condition==condition)&(trainlog.method==method)].iloc[0]
            assert logs.training_participants==text_indices([p for p in participants if p!=sub])
            assert str(logs.aligned_training_trials)==text_indices(np.flatnonzero(aligned)+1)
            block=candidates[(candidates.heldout_sub==sub)&(candidates.condition==condition)&(candidates.method==method)].sort_values('candidate_order')
            assert len(block)==len(pairs)
            for i,row in enumerate(block.itertuples()):
                assert (row.preprocessing,row.fix_id,row.sacc_id)==pairs[i]
                assert row.joint_available_rows==totals[i]
                if i in scores: close(row.J,scores[i])
                else: assert pd.isna(row.J)
                absent=text_indices(np.flatnonzero(train&~available[i])+1)
                assert ('' if pd.isna(row.unavailable_training_trials) else str(row.unavailable_training_trials))==absent
                assert bool(row.selected)==(i==winner)
            verified_candidates+=len(pairs)
print('Independent candidates, alignment, tie rules and all 160 selections PASS.',flush=True)
choices=table('policy_configuration_choices')
old=pd.concat([read('b_old'),read('h_old')],ignore_index=True)
fixed=read('fixed')
assert len(choices)==960
for r in choices.itertuples():
    assert defs.loc[r.configuration_id,'method']==r.method and defs.loc[r.configuration_id,'event']==r.event
    if r.selection_policy=='shared_preprocessing_joint':
        z=selection[(selection.heldout_sub==r.heldout_sub)&(selection.condition==r.condition)&(selection.method==r.method)].iloc[0]
        assert r.configuration_id==z['fix_id' if r.event=='fixation' else 'sacc_id']
    elif r.selection_policy=='existing_event_specific':
        z=old[(old.heldout_sub==r.heldout_sub)&(old.condition==r.condition)&(old.method==r.method)&(old.event==r.event)]
        assert z.configuration_id.nunique()==1 and z.configuration_id.iloc[0]==r.configuration_id
    else:
        if r.method=='IHMM': assert r.configuration_id==f'IHMM__{r.event}__sg__none'
        else:
            f=fixed[(fixed.method==r.method)&(fixed.event==r.event)].iloc[0]
            assert defs.loc[r.configuration_id,'smoothing']==f.smoothing and defs.loc[r.configuration_id,'parameter']==f.parameter

# Reproject saved intervals for every selected/fixed held-out call, independently of cached counts.
wanted=defaultdict(set)
for r in choices.itertuples():
    for key in keys:
        if key[0]==r.heldout_sub: wanted[r.condition,'h' if r.method=='IHMM' else 'b'].add((r.configuration_id,*key))
projected_calls=0
for (condition,family),targets in wanted.items():
    spans=defaultdict(list)
    for chunk in pd.read_csv(paths[f'{family}_events_{condition}'],chunksize=100000,float_precision='round_trip'):
        for r in chunk[['configuration_id','sub','Trial_Id','start_ms','end_ms']].itertuples(index=False,name=None):
            if r[:3] in targets: spans[r[:3]].append(r[3:])
    for cfg,sub,trial in targets:
        ti=key_index[sub,trial];ok,ct,status=calls[condition,cfg];intervals=spans[cfg,sub,trial]
        if not ok[ti]: assert not intervals;continue
        rows=mask[(mask['sub']==sub)&(mask.Trial_Id==trial)]
        times=rows.time_start.to_numpy();positive=rows.ground_truth.to_numpy()==defs.loc[cfg,'event'];prediction=np.zeros(len(rows),bool)
        for left,right in intervals: prediction|=(times>=left)&(times<=right)
        independent=np.array([sum(positive&prediction),sum(~positive&~prediction),sum(~positive&prediction),sum(positive&~prediction)])
        assert np.array_equal(independent,ct[ti]),(condition,cfg,sub,trial)
        projected_calls+=1
print('Saved-interval held-out count reconstruction PASS.',projected_calls,flush=True)

trialtable=table('heldout_trial_counts')
assert len(trialtable)==3*4*2*2*792
for r in trialtable.itertuples():
    ti=key_index[r.sub,r.Trial_Id];ok,ct,st=calls[r.condition,r.configuration_id]
    assert r.status==st[ti] and bool(r.call_available)==ok[ti]
    assert np.array_equal([r.TP,r.TN,r.FP,r.FN],ct[ti])
for (method,policy),group in trialtable.groupby(['method','selection_policy']):
    flags={}
    for (condition,event),g in group.groupby(['condition','event']):
        g=g.set_index(['sub','Trial_Id']).loc[keys];flags[condition,event]=g.call_available.to_numpy()
    paired=np.logical_and.reduce(list(flags.values()))
    assert np.array_equal(group.set_index(['condition','event','sub','Trial_Id']).loc[('original','fixation')].loc[keys].own_policy_paired_joint,paired)
    for r in group.itertuples():
        ti=key_index[r.sub,r.Trial_Id]
        assert r.condition_joint_available==(flags[r.condition,'fixation'][ti]&flags[r.condition,'saccade'][ti])
for (method,condition,event),group in trialtable.groupby(['method','condition','event']):
    policy_masks=[g.set_index(['sub','Trial_Id']).loc[keys].own_policy_paired_joint.to_numpy() for _,g in group.groupby('selection_policy')]
    shared=np.logical_and.reduce(policy_masks)
    for _,g in group.groupby('selection_policy'):assert np.array_equal(g.set_index(['sub','Trial_Id']).loc[keys].shared_policy_paired_joint,shared)
pooled=table('heldout_performance');participant=table('heldout_participant_performance')
for r in pooled.itertuples():
    g=trialtable[(trialtable.method==r.algorithm)&(trialtable.selection_policy==r.selection_policy)&(trialtable.condition==r.representation)&(trialtable.event==r.event)]
    support_column='condition_joint_available' if r.evaluation_support=='condition_specific_joint' else r.evaluation_support
    k=g[support_column].to_numpy();ct=g.loc[k,['TP','TN','FP','FN']].sum().to_numpy();close(metric(ct),[r.MCC,r.F1,r.precision,r.recall])
    assert np.array_equal(ct,[r.TP,r.TN,r.FP,r.FN])
    assert r.evaluation_rows==g.loc[k,'common_rows'].sum() and r.available_trials==g.condition_joint_available.sum()
    assert r.failed_trials==792-r.available_trials and r.scored_trials==k.sum()
    lookup=dict(zip(zip(g['sub'],g.Trial_Id),g[support_column]))
    ids=np.sort(mask.source_row_id.to_numpy()[np.array([lookup[t] for t in zip(mask['sub'],mask.Trial_Id)])])
    assert r.row_id_sha256==digest_ids(ids)
for r in participant.itertuples():
    g=trialtable[(trialtable.method==r.algorithm)&(trialtable.selection_policy==r.selection_policy)&(trialtable.condition==r.representation)&(trialtable.event==r.event)&(trialtable['sub']==r.sub)]
    support_column='condition_joint_available' if r.evaluation_support=='condition_specific_joint' else r.evaluation_support
    k=g[support_column].to_numpy();ct=g.loc[k,['TP','TN','FP','FN']].sum().to_numpy();close(metric(ct),[r.MCC,r.F1,r.precision,r.recall])
    assert np.array_equal(ct,[r.TP,r.TN,r.FP,r.FN]) and r.evaluation_rows==g.loc[k,'common_rows'].sum()
cnn=table('cnn_five_branch_performance')
for condition in ['original','rate30']:
    for event in ['fixation','saccade']:
        frame=read(f'cnn_predictions_{event}_{condition}')
        assert len(frame)==68577 and frame.source_row_id.is_unique
        for branch in cnn.branch.unique():
            p=frame['CNN_'+branch].to_numpy().astype(bool);y=frame.truth.to_numpy().astype(bool)
            ct=np.array([sum(y&p),sum(~y&~p),sum(~y&p),sum(y&~p)])
            z=cnn[(cnn.branch==branch)&(cnn.representation==condition)&(cnn.event==event)].iloc[0]
            close(metric(ct),[z.MCC,z.F1,z.precision,z.recall]);assert np.array_equal(ct,z[['TP','TN','FP','FN']].to_numpy())
            assert frame['CNN_'+branch+'_available'].all() and z.evaluation_rows==68577
for frame in [pooled,cnn]:
    keys2=['algorithm','representation','selection_policy']+(['evaluation_support'] if 'evaluation_support' in frame else [])+(['branch'] if 'branch' in frame else [])
    for _,g in frame.groupby(keys2):
        assert len(g)==2;close(g.joint_descriptive_mean_MCC,np.repeat(g.MCC.mean(skipna=False),2))
ledger=pd.read_csv(output/'validation/plot_ledger.csv')
assert len(ledger)==120 and ledger.figure.nunique()==4
for r in ledger.itertuples():
    src=pd.read_csv(output/f'validation/plot_source_{r.table}.csv').iloc[r.source_row-1]
    close(src[r.metric],r.value);assert src.evaluation_rows==r.evaluation_rows
for row in registry.itertuples(): assert sha(row.path)==row.sha256,row.id
report=dict(passed=True,outer_folds_verified=len(selection),candidate_pairs_verified=verified_candidates,
            heldout_interval_calls_reconstructed=projected_calls,trial_records=len(trialtable),pooled_records=len(pooled),
            CNN_branches=5,participants=20,training_test_separation=True,aligned_training_support=True,
            deterministic_selection=True,independent_metrics=True,shared_policy_support=True,inputs_unchanged=True,
            primary_hmm_seed_id=0,no_model_or_detector_calls=True,plotted_values=len(ledger),figures=4)
(output/'validation/independent_verification.json').write_text(json.dumps(report,indent=2))
print('Independent Part 2 verification PASS',json.dumps(report),flush=True)
