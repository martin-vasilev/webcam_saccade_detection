"""Independent full-development selector and read-only native-event inventory.

No detector, neural model or scientific event-level endpoint is computed here.
Counts below are integrity/availability counts, not event-distribution analyses.
"""
from pathlib import Path
from collections import Counter
import hashlib, itertools, json, sys
sys.dont_write_bytecode=True
import numpy as np
import pandas as pd

root=Path(sys.argv[1]).resolve();w=root.parents[1];out=root/'output/part2b'
def sha(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda:f.read(4194304),b''):h.update(b)
    return h.hexdigest()
registry=pd.read_csv(root/'config/part2b/input_manifest.csv').fillna('')
paths=dict(zip(registry.id,registry.path))
for condition in ['original','rate30']:
    # Path.stem on an archived .csv.gz leaves the .csv suffix in its registry ID.
    alias=f'cnn_{condition}_all_native_events'
    if alias not in paths:paths[alias]=paths[alias+'.csv']
def preserve():
    for row in registry.itertuples():assert sha(row.path)==row.sha256,row.id
preserve()
def read(id):return pd.read_csv(paths[id],float_precision='round_trip')
def table(name):return pd.read_csv(out/(name+'.csv'),float_precision='round_trip')
def close(a,b):np.testing.assert_allclose(np.asarray(a,dtype=float),np.asarray(b,dtype=float),rtol=1e-11,atol=1e-12,equal_nan=True)
def metrics(c):
    tp,tn,fp,fn=np.asarray(c,dtype=float)
    den=(tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
    divide=lambda a,b:a/b if b else np.nan
    return np.array([divide(tp*tn-fp*fn,np.sqrt(den)),divide(2*tp,2*tp+fp+fn),divide(tp,tp+fp),divide(tp,tp+fn)])
def identity(ids):return hashlib.sha256(','.join(map(str,sorted(ids))).encode()).hexdigest()
trials=read('trials');keys=list(zip(trials['sub'],trials.Trial_Id));key_index={k:i for i,k in enumerate(keys)}
assert len(keys)==len(set(keys))==792
mask=read('mask');mask=mask[mask.common_eligible].copy()
assert len(mask)==68577 and mask.source_row_id.is_unique
mi=np.array([key_index[k] for k in zip(mask['sub'],mask.Trial_Id)])
weights=np.bincount(mi,minlength=792)
definitions=read('b_grid');hd=read('h_grid');hd['threshold_value']=np.nan
hd['preprocessing']=hd.smoothing.map(dict(raw='Raw',mean='Mean3',median='Median3',sg='SG5_23'))
defs=pd.concat([definitions,hd],ignore_index=True).set_index('configuration_id')
bc=read('b_counts');hc=read('h_counts');calls={}
for family,cache in [('b',bc),('h',hc)]:
    for condition in ['original','rate30']:
        ledger=read(f'{family}_status_{condition}').set_index(['configuration_id','sub','Trial_Id'])
        for cfg,g in cache[cache.condition==condition].groupby('configuration_id'):
            g=g.set_index(['sub','Trial_Id']).loc[keys]
            assert len(g)==792 and g.index.is_unique
            st=ledger.loc[[(cfg,*k) for k in keys]]
            assert np.array_equal(st.status.to_numpy(),g.status.to_numpy())
            ok=g.status.isin(['success','success_empty']).to_numpy()
            counts=g[['TP','TN','FP','FN']].to_numpy().astype(float);counts[~ok]=0
            assert np.array_equal(g.common_rows,weights)
            assert np.array_equal(counts[ok].sum(axis=1),weights[ok])
            truth=np.bincount(mi[mask.ground_truth.to_numpy()==defs.loc[cfg,'event']],minlength=792)
            assert np.array_equal(counts[ok,0]+counts[ok,3],truth[ok])
            calls[condition,cfg]=(ok,counts,g.status.to_numpy())
assert len(calls)==656
print('Candidate availability, explicit source statuses and reference-row sufficiency PASS.',flush=True)

audit=table('configuration_selection_audit');final=table('final_fixed_configurations');classical=final[final.algorithm!='CNN-BLSTM']
support=table('development_support');lopo=pd.read_csv(root/'output/part2/tables/selected_configurations.csv')
stability=table('configuration_stability');verified=0;selected_support={}
for condition in ['original','rate30']:
    for method in ['IVT','IDT','EK03','IHMM']:
        pairs=[]
        for pre in ['Raw','Mean3','Median3','SG5_23']:
            d=defs[(defs.method==method)&(defs.preprocessing==pre)]
            f=d[d.event=='fixation'].sort_values('threshold_value',kind='stable')
            s=d[d.event=='saccade'].sort_values('threshold_value',kind='stable')
            pairs.extend((pre,fi,si) for fi,si in itertools.product(f.index,s.index))
        avail=np.column_stack([calls[condition,f][0]&calls[condition,s][0] for _,f,s in pairs])
        n=weights@avail;top=np.flatnonzero(n==n.max());aligned=avail[:,top].all(axis=1)
        rowids=mask.source_row_id.to_numpy()[aligned[mi]];h=identity(rowids);score={}
        for i in top:
            _,f,s=pairs[i];fm=metrics(calls[condition,f][1][aligned].sum(axis=0))[0];sm=metrics(calls[condition,s][1][aligned].sum(axis=0))[0]
            score[i]=(fm,sm,(fm+sm)/2)
        admissible=[i for i in top if np.isfinite(score[i][2])]
        best=max(score[i][2] for i in admissible)
        winner=next(i for i in admissible if score[i][2]>=best-1e-12)
        ranked=sorted(admissible,key=lambda i:(-score[i][2],i))
        a=audit[(audit.algorithm==method)&(audit.representation==condition)].sort_values('candidate_order')
        assert len(a)==len(pairs)
        for i,r in enumerate(a.itertuples()):
            pre,f,s=pairs[i]
            assert (r.candidate_order,r.preprocessing,r.fix_id,r.sacc_id)==(i+1,pre,f,s)
            close([r.fix_threshold,r.sacc_threshold],[defs.loc[f,'threshold_value'],defs.loc[s,'threshold_value']])
            assert r.jointly_available_rows==n[i] and r.maximum_available_rows==n.max()
            assert r.joint_available_trials==avail[:,i].sum()
            indices=','.join(map(str,np.flatnonzero(~avail[:,i])+1))
            assert ('' if pd.isna(r.unavailable_trial_indices) else str(r.unavailable_trial_indices))==indices
            assert r.maximum_availability==(i in top) and r.selected==(i==winner)
            assert r.aligned_rows==len(rowids) and r.aligned_row_sha256==h
            close([r.fixation_MCC,r.saccade_MCC,r.J],score.get(i,[np.nan]*3))
            close(r.objective_rank,ranked.index(i)+1 if i in ranked else np.nan)
            assert r.tie_with_best==(i in admissible and score[i][2]>=best-1e-12)
            reason='selected' if i==winner else 'excluded_lower_availability' if i not in top else 'excluded_undefined_MCC' if i not in admissible else 'eligible_not_selected'
            assert r.disposition==reason
        f=classical[(classical.algorithm==method)&(classical.representation==condition)].iloc[0]
        pre,fi,si=pairs[winner]
        assert (f.preprocessing,f.fix_id,f.sacc_id)==(pre,fi,si)
        close([f.development_fixation_MCC,f.development_saccade_MCC,f.development_J],score[winner])
        assert f.development_evaluation_rows==len(rowids) and f.development_row_sha256==h
        p=support[(support.algorithm==method)&(support.representation==condition)].iloc[0]
        assert p.aligned_trial_indices==','.join(map(str,np.flatnonzero(aligned)+1)) and p.row_id_sha256==h
        assert list(map(int,p.participants.split(',')))==sorted(trials['sub'].unique())
        q=lopo[(lopo.method==method)&(lopo.condition==condition)]
        assert len(q)==20 and q.heldout_sub.nunique()==20
        t=stability[(stability.algorithm==method)&(stability.representation==condition)].iloc[0]
        freqs=Counter(zip(q.fix_id,q.sacc_id));modal=max(freqs.values());modes={k for k,v in freqs.items() if v==modal}
        assert t.folds_same_preprocessing==sum(q.preprocessing==pre)
        assert t.folds_exact_configuration==freqs.get((fi,si),0)
        assert t.final_agrees_with_any_mode==((fi,si) in modes) and t.modal_configuration_frequency==modal and t.mode_tied==(len(modes)>1)
        assert t.modal_configuration_ids==';'.join(sorted(f'{f}|{s}' for f,s in modes))
        assert t.preprocessing_frequencies==';'.join(f'{v}:{sum(q.preprocessing==v)}' for v in sorted(set(q.preprocessing)))
        for column,prefix in [('fix_threshold','fixation'),('sacc_threshold','saccade')]:
            z=q[column].dropna().to_numpy();close([t[prefix+'_threshold_min'],t[prefix+'_threshold_max'],t[prefix+'_threshold_sd']],
                [z.min(),z.max(),z.std(ddof=1)] if len(z) else [np.nan]*3)
            assert t[prefix+'_threshold_unique']==len(np.unique(z))
            expected=';'.join(f'{v:g}:{sum(z==v)}' for v in sorted(set(z))) if len(z) else 'NA:20'
            actual=t[prefix+'_threshold_frequencies']
            assert actual==expected,(actual,expected)
        selected_support[method,condition]=aligned
        verified+=len(pairs)
assert verified==len(audit)==4344
print('Independent development ranking, aligned support and all eight fixed selections PASS.',flush=True)

performance=table('performance_interpretation')
dev=performance[performance.estimate_type=='development_selected_fixed_in_sample']
assert len(dev)==16
for r in dev.itertuples():
    k=selected_support[r.algorithm,r.representation]
    c=calls[r.representation,r.configuration_id][1][k].sum(axis=0)
    close(c,[r.TP,r.TN,r.FP,r.FN]);close(metrics(c),[r.MCC,r.F1,r.precision,r.recall]);assert r.evaluation_rows==weights[k].sum()
for oldname,kind in [('heldout_performance','policies'),('cnn_five_branch_performance','cnn')]:
    old=pd.read_csv(root/f'output/part2/tables/{oldname}.csv',float_precision='round_trip')
    if kind=='policies':new=performance[performance.estimate_type.isin(['historical_fixed_benchmark','outer_LOPO_policy_heldout_not_final_fixed_configuration'])]
    else:new=performance[performance.estimate_type=='fixed_CNN_branch_outer_participant_heldout_no_branch_selector']
    assert len(new)==len(old)
    close(new[['TP','TN','FP','FN','MCC','F1','precision','recall','evaluation_rows']],old[['TP','TN','FP','FN','MCC','F1','precision','recall','evaluation_rows']])
cnn=table('cnn_five_branch_performance');ranking=table('cnn_branch_ranking')
pd.testing.assert_frame_equal(cnn,pd.read_csv(root/'output/part2/tables/cnn_five_branch_performance.csv',float_precision='round_trip'))
for r in ranking.itertuples():
    g=cnn[(cnn.branch==r.branch)&(cnn.representation==r.representation)]
    close([r.fixation_MCC,r.saccade_MCC,r.J],[g[g.event=='fixation'].MCC.iloc[0],g[g.event=='saccade'].MCC.iloc[0],g.MCC.mean()])
    expected=ranking[ranking.representation==r.representation].J.rank(ascending=False,method='min').loc[r.Index]
    assert r.descriptive_rank==expected and r.observed_highest==(expected==1)
assert len(final)==10 and set(final[final.algorithm=='CNN-BLSTM'].preprocessing)=={'Raw'}
assert final[final.algorithm=='CNN-BLSTM'].development_J.isna().all()

# Native source geometry. Original IDs are the zero-based data-frame rows,
# not the printed R row-name columns. Classical Original applies conf/pupil validity.
truth=read('truth');truth['source_row_id']=np.arange(len(truth))
matched=truth.iloc[mask.source_row_id.to_numpy()]
assert np.array_equal(matched['sub'].to_numpy(),mask['sub']) and np.array_equal(matched.Trial_Id.to_numpy(),mask.Trial_Id)
assert np.array_equal(matched.ground_truth.to_numpy(),mask.ground_truth)
close(matched.time_start.to_numpy(),mask.time_start)
original=truth[(truth.conf>0)&(truth.el_pupil>0)].copy()
orig_groups={k:g for k,g in original.groupby(['sub','Trial_Id'],sort=False)}
coords30={}
for r in registry.itertuples():
    if r.id.startswith('coordinates_'):
        g=pd.read_csv(r.path,float_precision='round_trip');assert g.source_row_id.is_unique
        key=(int(g['sub'].iloc[0]),int(g.Trial_Id.iloc[0]));coords30[key]=g
assert set(coords30)==set(keys)
native30=read('cnn_native_rate30').set_index('source_row_id')
assert native30.index.is_unique
reference_groups={k:g for k,g in mask.groupby(['sub','Trial_Id'])}
readiness=[];projection_calls=0
for condition in ['original','rate30']:
    coordinate_groups=orig_groups if condition=='original' else coords30
    for family in ['b','h']:
        chosen=classical[(classical.representation==condition)&((classical.algorithm=='IHMM') if family=='h' else (classical.algorithm!='IHMM'))]
        ids=set(chosen.fix_id)|set(chosen.sacc_id)
        parts=[]
        for chunk in pd.read_csv(paths[f'{family}_events_{condition}'],chunksize=100000,float_precision='round_trip'):
            parts.append(chunk[chunk.configuration_id.isin(ids)])
        events=pd.concat(parts,ignore_index=True)
        statuses=read(f'{family}_status_{condition}')
        for row in chosen.itertuples():
            for event,cfg in [('fixation',row.fix_id),('saccade',row.sacc_id)]:
                e=events[events.configuration_id==cfg]
                s=statuses[statuses.configuration_id==cfg].set_index(['sub','Trial_Id']).loc[keys]
                assert len(s)==792 and s.index.is_unique
                assert set(zip(e['sub'],e.Trial_Id))<=set(keys)
                close(e.end_ms-e.start_ms,e.duration)
                assert np.isfinite(e[['start_ms','end_ms','duration']]).all().all() and (e.duration>=0).all()
                by={k:g for k,g in e.groupby(['sub','Trial_Id'])}
                endpoint_missing=0
                aligned=selected_support[row.algorithm,condition]
                for ti,key in enumerate(keys):
                    z=by.get(key,e.iloc[:0]);st=s.loc[key];ok=st.status in ['success','success_empty']
                    assert len(z)==st.n_events
                    if not ok:assert len(z)==0
                    if st.status=='success_empty':assert len(z)==0
                    if len(z):
                        times=coordinate_groups[key].time_start.to_numpy()
                        endpoints=z[['start_ms','end_ms']].to_numpy().ravel()
                        loc=np.searchsorted(times,endpoints)
                        nearest=np.minimum(np.abs(times[np.clip(loc,0,len(times)-1)]-endpoints),np.abs(times[np.clip(loc-1,0,len(times)-1)]-endpoints))
                        endpoint_missing+=int((nearest>1e-7).sum())
                    if ok and aligned[ti]:
                        ref=reference_groups.get(key,mask.iloc[:0]);t=ref.time_start.to_numpy();y=ref.ground_truth.to_numpy()==event;p=np.zeros(len(t),bool)
                        for zrow in z.itertuples():p|=(t>=zrow.start_ms)&(t<=zrow.end_ms)
                        c=[sum(y&p),sum(~y&~p),sum(~y&p),sum(y&~p)]
                        close(c,calls[condition,cfg][1][ti]);projection_calls+=1
                assert endpoint_missing==0,(cfg,condition,endpoint_missing)
                successes=s.status.isin(['success','success_empty'])
                readiness.append(dict(algorithm=row.algorithm,representation=condition,branch='',event=event,configuration_id=cfg,
                    event_path=paths[f'{family}_events_{condition}'],status_path=paths[f'{family}_status_{condition}'],
                    event_filter=f'configuration_id == {cfg}',events_verified=len(e),trial_roster=792,participants=20,
                    successful_trials=int(successes.sum()),failed_or_unavailable_trials=int((~successes).sum()),successful_zero_event_trials=int((successes&(s.n_events==0)).sum()),
                    native_start_column='start_ms',native_end_column='end_ms',duration_column='duration',duration_definition='end_ms - start_ms; inclusive sample endpoints',
                    source_mapping='Original: zero-based truth source rows filtered conf>0 & el_pupil>0' if condition=='original' else 'archived rate30/inputs/sub{ID}_trial{ID}.csv source_row_id,time_start,x,y',
                    source_coordinate_path=paths['truth'] if condition=='original' else str(w/'sampling_rate_analysis/full_analysis_a_001/rate30/inputs'),
                    source_ids_embedded=False,coordinate_mapping_verified=True,missing_endpoint_mappings=endpoint_missing,
                    explicit_success_empty_status=True,compatible_reference_path=paths['mask'],
                    readiness='available_native_events_with_explicit_status',remaining_decision='reference native segmentation/support and duration convention must be frozen before Part 3; velocities absent',
                    exclusive_multiclass=False))
print('Selected native classical events, endpoint maps, statuses and development count reprojection PASS.',projection_calls,flush=True)

for condition in ['original','rate30']:
    e=read(f'cnn_{condition}_all_native_events');s=read(f'cnn_{condition}_all_trial_status')
    assert len(s)==5*792 and not s.duplicated(['branch','sub','Trial_Id']).any()
    close(e.last_sample_ms-e.start_ms,e.sample_span_ms);close(e.sample_span_ms+e.median_step_ms,e.duration_ms)
    assert (e.n_samples>=1).all() and (e.sample_span_ms>=0).all()
    for column,time in [('first_source_id','start_ms'),('last_source_id','last_sample_ms')]:
        source=truth.iloc[e[column].to_numpy().astype(int)]
        assert np.array_equal(source['sub'].to_numpy(),e['sub']) and np.array_equal(source.Trial_Id.to_numpy(),e.Trial_Id)
        close(source.time_start.to_numpy(),e[time])
        if condition=='rate30':
            mapped=native30.loc[e[column].to_numpy()];close(mapped.time_start.to_numpy(),e[time])
    for branch in ['raw','mean','median','sg_p3_n7','sg_p5_n23']:
        st=s[s.branch==branch].set_index(['sub','Trial_Id']).loc[keys]
        all_events=e[e.branch==branch]
        # All native class runs jointly account for every native prediction sample.
        counts=all_events.groupby(['sub','Trial_Id']).n_samples.sum().reindex(keys,fill_value=0)
        assert np.array_equal(counts,st.n_native) and (st.missing_predictions==0).all()
        for event,label in [('fixation',1),('saccade',2)]:
            ev=all_events[all_events.label==label]
            sample_counts=ev.groupby(['sub','Trial_Id']).n_samples.sum().reindex(keys,fill_value=0)
            assert np.array_equal(sample_counts,st[event+'_samples'])
            assert np.array_equal(st['successful_no_'+event+'_events'],sample_counts==0)
            ok=st.status=='success'
            readiness.append(dict(algorithm='CNN-BLSTM',representation=condition,branch=branch,event=event,configuration_id='CNN_'+branch,
                event_path=paths[f'cnn_{condition}_all_native_events'],status_path=paths[f'cnn_{condition}_all_trial_status'],
                event_filter=f'branch == {branch}, label == {label}',events_verified=len(ev),trial_roster=792,participants=20,
                successful_trials=int(ok.sum()),failed_or_unavailable_trials=int((~ok).sum()),successful_zero_event_trials=int((ok&(sample_counts==0)).sum()),
                native_start_column='start_ms',native_end_column='last_sample_ms',duration_column='duration_ms',
                duration_definition='sample_span_ms + trial median_step_ms; sample_span_ms = last_sample_ms - start_ms',
                source_mapping='embedded first_source_id,last_source_id; exact zero-based truth source row and time match; 30 Hz additionally matched native_input.csv',
                source_coordinate_path=paths['truth'] if condition=='original' else paths['cnn_native_rate30'],
                source_ids_embedded=True,coordinate_mapping_verified=True,missing_endpoint_mappings=0,explicit_success_empty_status=True,
                compatible_reference_path=paths['mask'],readiness='available_native_predicted_class_runs_not_reference_time_projected_runs',
                remaining_decision='CNN primary branch provisional; cross-method span versus reported duration, native reference segmentation/support and velocity basis not yet frozen',exclusive_multiclass=True))
ready=pd.DataFrame(readiness);assert len(ready)==36
ready.to_csv(out/'event_input_readiness.csv',index=False)
# Export exact historical arguments, without changing the selected configurations.
full_definitions=[]
for f in classical.itertuples():
    for event,cfg in [('fixation',f.fix_id),('saccade',f.sacc_id)]:
        definition=defs.loc[cfg].to_dict()
        definition.update(configuration_id=cfg,representation=f.representation,event=event)
        if f.algorithm=='IHMM':
            definition.update(nstates=2,min_fix_dur_argument_ms=50,min_sacc_amplitude_deg=.15,
                seed_id=0,seed_policy='preserved per-configuration/trial seeds in C1 execution ledger; no refit',
                dva_x=.01865554,dva_y=.01919689,
                parameter_provenance=paths['detector_source_IHMM'])
        full_definitions.append(definition)
pd.DataFrame(full_definitions).to_csv(out/'selected_configuration_definitions.csv',index=False)
# A per-trial roster supplements compact availability summaries, preserving empty/failure distinctions.
print('All five CNN native event exports, durations, source anchors and trial rosters PASS.',flush=True)

def markdown(df):
    f=df.copy()
    for c in f:
        if f[c].dtype.kind=='f':f[c]=f[c].map(lambda x:'' if pd.isna(x) else f'{x:.4f}')
    return '| '+' | '.join(f.columns)+' |\n| '+' | '.join(['---']*len(f.columns))+' |\n'+'\n'.join('| '+' | '.join(str(x).replace('|','/') for x in r)+' |' for r in f.itertuples(index=False,name=None))
decision='''# CNN branch decision — provisional, no learned selector

All five branches retain their independently verified outer-participant-held-out predictions. Their branch-specific scores are valid descriptions of those fixed training pipelines on the frozen common support (68,577 rows per event/rate). The joint column is the arithmetic mean of fixation and saccade pooled MCC. Ranking these already inspected results is post hoc; the performance of choosing the observed winner is not independently validated.

'''+markdown(ranking.sort_values(['representation','descriptive_rank'])[['representation','branch','fixation_MCC','saccade_MCC','J','descriptive_rank']])+'''

The highest observed joint-MCC branch is Mean3 at Original and Raw at 30 Hz. These are descriptive rankings, not unbiased selected-model estimates.

For practical downstream bookkeeping, propose **Raw at both rates as a provisional primary**, because it requires no smoothing transformation and keeps the preprocessing definition constant across representations. This rationale concerns interpretability, not demonstrated superiority or an assertion that Raw is outcome-neutral. The recommendation is made after the branch outcomes were known, and Raw happens to be the observed 30-Hz winner. It therefore requires explicit scientific confirmation and must remain labelled provisional. All four other branches should remain prespecified sensitivity outputs, with the complete five-branch table available.

Alternatives are to keep all five branches co-primary (avoids compressing the present evidence), adopt an externally justified fixed smoother at both rates, or conduct a new nested/independent branch-selection study. Common SG5/23 may aid nominal consistency but changes physical smoothing duration by rate and is not scientifically neutral. Using rate-specific observed winners can be shown only as a post hoc sensitivity. No option is established as optimal by the available data alone.

Do not treat other participants' existing LOPO predictions as an inner validation set: their models generally included the current outer test participant. No inner branch selection, retraining or inference was performed. CNN native events are preserved predicted class runs, not reconstructed runs from the Original-time categorical projection. The 30-Hz Option A training budget differs from Original, which remains a cross-rate interpretation limitation.
'''
(out/'cnn_branch_decision.md').write_text(decision,encoding='utf-8')
status='''# Phase 2B status — COMPLETE / PASS, CNN primary provisional

## Final development-selected classical settings

The full set of 20 participants and 792 trial identities was used for development selection; 68,577 frozen original-timestamp rows form the eligible geometry before failures. These scores use development labels and are **not independent held-out estimates of the exact final fixed configurations**.

'''+markdown(classical[['algorithm','representation','preprocessing','fix_threshold','sacc_threshold','development_evaluation_rows','development_fixation_MCC','development_saccade_MCC','development_J']])+'''

The same reliability-first policy was applied at both rates: maximise joint available development rows; intersect jointly successful trials across all maximum-availability candidates; score both tasks on this identical explicitly recorded support; maximise the equal-event mean of pooled MCC. Undefined task MCC makes a candidate inadmissible, without lower-availability fallback. Within 1e-12 of the best objective, use Raw, Mean3, Median3, SG5/23 then ascending fixation/saccade thresholds. Candidate audit records lower-availability exclusions with undefined/uncomputed objective fields, not invented scores. Exact J ranks and tolerance-tie selection outcomes are separate fields. I-HMM retains primary seed schedule 0 and existing separate event fits. No seed optimisation or multiclass reconciliation occurred.

## Stability, not a selection rule

'''+markdown(stability[['algorithm','representation','folds_same_preprocessing','folds_exact_configuration','modal_configuration_frequency','final_agrees_with_any_mode']])+'''

The stability file also contains preprocessing and threshold frequencies, threshold ranges/sample SD, tied modal identities and exact final configuration IDs. These describe the twenty prior LOPO selections. They did not determine the full-development winner.

## Estimates that must stay separate

`performance_interpretation.csv` labels (A) in-sample development-selected fixed estimates, (B) outer-LOPO held-out selection-policy estimates, and (C) historical fixed and existing event-specific policy benchmarks. No held-out policy score is attached to the exact final fixed configuration. The preserved Phase 2A policies retain their own declared evaluation masks; scores on different masks are not interpreted as controlled improvements. CNN five-branch held-out values remain separate, with post hoc rankings in `cnn_branch_ranking.csv`. The provisional common Raw recommendation is explained in `cnn_branch_decision.md`; it is not a validated optimal-branch selection procedure.

## Event input readiness and remaining scientific decisions

All 16 selected classical event/configuration/rate combinations and all 20 CNN branch/event/rate combinations have saved native intervals and explicit 792-trial rosters. `event_input_readiness.csv` gives exact paths, filters, columns, duration definitions, coordinate joins, empty/failure accounting and reference paths. Event counts in that inventory are integrity checks only. `selected_configuration_definitions.csv` includes the exact grid definitions, threshold units, fixed window settings and HMM defaults/seed provenance. Classical intervals use end minus start; CNN reporting duration adds the trial median step to the first-to-last sample span. Zero-target successful trials are distinguished from failed calls. Native endpoints map to archived coordinates, and CNN source anchors match exact Original source identities and timestamps. Native data must not be replaced by projected Original-time label runs.

The compatible sample-level reference is the frozen original-timestamp manual-label geometry. Native reference **event segmentation**, treatment of removed/blocked rows, duration harmonisation, zero/singleton handling and event-level comparison support still need to be frozen for Part 3. The raw manual-interval table uses a different clock/trial system and is not established here as a directly joinable substitute. Peak/mean velocities are absent from compact classical/CNN event exports; any later reconstruction requires an explicit derivative, coordinate basis and boundary rule. No new event statistics, velocity or Wasserstein calculations were performed. The two event-specific classical calls remain binary streams and may overlap or leave uncovered time; no exclusive timeline is implied.

## Validation and preservation

Independent Python validation rebuilt the candidate ordering, maximum availability, aligned reference identities, objective, ties, eight selected configurations, stability summaries, confusion metrics and CNN rankings from pinned source ledgers. Saved selected classical intervals independently reproduced development counts on the aligned support. Native event/status/coordinate and CNN duration/source-anchor checks passed. Inputs, all Part 1 and Phase 2A outputs, manifests and archived implementations were rehashed unchanged. Only consumed source artifacts and earlier result bundles were rehashed; this is not a new exhaustive audit of every unconsumed model checkpoint. No detector, HMM fitting, neural inference/training or new statistical test was called.

Part 1 and Part 2 invocation compatibility is maintained using byte-identical archived entry points. Their existing manifests remain unchanged. The new dispatcher is pinned by the Part 2B output manifest. Session/package information and the independent verification log are saved under `validation/`.

The initial independent pass verified classical selection and saved-interval counts before encountering a registry alias error for a compressed CNN CSV. The alias was corrected in this new verifier; frozen data and selected settings were unchanged. The initial diagnostic log is retained outside the result bundle. R locale startup warnings do not alter numeric results.

## Reproduction and next step

From this extension directory: `Rscript run_results.R --stage part2b`. A completed bundle is hash-verified and reused. In a separately versioned clean copy with the pinned configuration and source archive, the same command regenerates development selection and verification. Preserve existing completed outputs. `--stage part1` and `--stage part2` continue to verify/reuse their original bundles. The bootstrap script was used once to establish pins and is not a routine rerun step.

Phase 2B implementation is complete with no unresolved numerical integrity blocker. Confirm the provisional CNN primary/sensitivity arrangement and the event/reference conventions before separately authorising Part 3. No manuscript, production, historical result or Git synchronisation change was made; Parts 3–4 remain unstarted.
'''
(out/'PHASE2B_STATUS.md').write_text(status,encoding='utf-8')
preserve()
gate=dict(passed=True,source_records=len(registry),development_candidate_pairs_verified=verified,
    selected_classical_configurations=8,provisional_CNN_rows=2,all_CNN_branches=5,
    development_interval_calls_reprojected=projection_calls,native_event_inventory_rows=len(ready),
    reference_rows=68577,trial_roster=792,participant_count=20,
    exact_development_scores_not_heldout=True,selection_stability_verified=True,
    candidate_statuses_and_support_verified=True,earlier_outputs_unchanged=True,
    CNN_decision_requires_scientific_confirmation=True,event_level_definitions_not_yet_frozen=True,
    no_detector_training_inference_or_event_statistics=True)
(out/'validation/independent_verification.json').write_text(json.dumps(gate,indent=2))
print('Independent Phase 2B verification PASS',json.dumps(gate),flush=True)
