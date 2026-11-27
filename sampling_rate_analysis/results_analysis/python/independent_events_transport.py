"""Independent native-event, summary, velocity and exact-transport validation."""
from pathlib import Path
from collections import defaultdict
import hashlib,json,sys
sys.dont_write_bytecode=True
import numpy as np
import pandas as pd
import scipy
from scipy.signal import savgol_filter
from scipy.spatial.distance import cdist
from scipy.optimize import linear_sum_assignment
root=Path(sys.argv[1]).resolve();out=root/'output/part3';pretransport_only='--pretransport' in sys.argv[2:]
registry=pd.read_csv(root/'config/part3/input_manifest.csv');paths=dict(zip(registry.id,registry.path))
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
def preserve():
 for r in registry.itertuples():assert sha(r.path)==r.sha256,r.id
def read(p):return pd.read_csv(p,float_precision='round_trip')
def close(a,b,atol=1e-7,rtol=1e-9):np.testing.assert_allclose(np.asarray(a,dtype=float),np.asarray(b,dtype=float),atol=atol,rtol=rtol,equal_nan=True)
preserve()
(out/'validation'/('python_environment_pretransport.json' if pretransport_only else 'python_environment.json')).write_text(json.dumps(dict(python=sys.version,numpy=np.__version__,pandas=pd.__version__,scipy=scipy.__version__,
  velocity_absolute_tolerance=2e-5,velocity_relative_tolerance=1e-7,transport_absolute_tolerance=2e-6,transport_relative_tolerance=1e-7),indent=2))
events=read(out/'derived/native_events.csv');roster=read(out/'derived/call_roster.csv');coord=read(out/'derived/coordinates.csv')
summary=read(out/'event_trial_summary.csv');truth=read(paths['truth']);truth['source_row_id']=np.arange(len(truth))
trials=read(paths['trials']);keys=list(zip(trials['sub'],trials.Trial_Id));keytext=[f'{s}/{t}' for s,t in keys]
assert len(keys)==len(set(keys))==792 and trials['sub'].nunique()==20
assert events.event_id.is_unique and not roster.duplicated(['model','representation','reference_definition','event','sub','Trial_Id']).any()
assert (events.native_duration_ms>=0).all();close(events.last_sample_ms-events.start_ms,events.span_ms)
fixed=read(root/'output/part2b/final_fixed_configurations.csv')
for f in fixed[fixed.algorithm!='CNN-BLSTM'].itertuples():
 for event,cfg in [('fixation',f.fix_id),('saccade',f.sacc_id)]:
  q=roster[(roster.model==f.algorithm)&(roster.representation==f.representation)&(roster.event==event)]
  assert len(q)==792 and set(q.configuration_id)=={cfg}
  q=events[(events.model==f.algorithm)&(events.representation==f.representation)&(events.event==event)]
  assert set(q.configuration_id)=={cfg}
classical_or_reference=events[~events.model.str.startswith('CNN_')]
close(classical_or_reference.native_duration_ms,classical_or_reference.span_ms)
groups={k:g.sort_values('time_start') for k,g in coord.groupby(['representation','sub','Trial_Id'])}
cnncoord=read(out/'derived/cnn_original_coordinates.csv')
cnngroups={k:g.sort_values('time_start') for k,g in cnncoord.groupby(['representation','sub','Trial_Id'])}
edges=read(paths['blocked_edges']);eligible=dict(zip(zip(edges.left_source_row_id,edges.right_source_row_id),edges.eligible))
for (condition,s,t),g in groups.items():
 source=truth.iloc[g.source_row_id.to_numpy()]
 close(source[['time_start','x','y']],g[['time_start','x','y']]);assert (np.diff(g.time_start)>0).all()
 assert (source['sub'].to_numpy()==s).all() and (source.Trial_Id.to_numpy()==t).all()
 if condition=='original':
  expected=truth[(truth['sub']==s)&(truth.Trial_Id==t)&(truth.conf>0)&(truth.el_pupil>0)].sort_values('time_start')
  assert np.array_equal(g.source_row_id,expected.source_row_id)
  components=np.r_[0,np.cumsum([not eligible[a,b] for a,b in zip(g.source_row_id.iloc[:-1],g.source_row_id.iloc[1:])])]
  assert np.array_equal(components,g.component_id)
 else:
  original=groups['original',s,t].set_index('source_row_id')
  assert np.array_equal(original.loc[g.source_row_id].component_id,g.component_id)
for (_,s,t),g in cnngroups.items():
 expected=truth[(truth['sub']==s)&(truth.Trial_Id==t)].sort_values('time_start')
 assert np.array_equal(g.source_row_id,expected.source_row_id)
 close(expected[['time_start','x','y']],g[['time_start','x','y']])
 valid_map=groups['original',s,t].set_index('source_row_id').component_id.to_dict()
 assert np.array_equal(g.component_id,[valid_map.get(i,-i-1) for i in g.source_row_id])

# Check every native saved record and independently rebuild reference boundaries.
source_events={p:read(p) for p in events.loc[events.model!='GT','source_path'].unique()}
for p,g in events[events.model!='GT'].groupby('source_path'):
 source=source_events[p].iloc[g.source_record.to_numpy().astype(int)]
 end='last_sample_ms' if 'label' in source else 'end_ms';dur='duration_ms' if 'label' in source else 'duration'
 close(source.start_ms,g.start_ms);close(source[end],g.last_sample_ms);close(source[dur],g.native_duration_ms)
 assert np.array_equal(source['sub'].to_numpy(),g['sub']) and np.array_equal(source.Trial_Id.to_numpy(),g.Trial_Id)
 if 'label' in source:
  assert np.array_equal(source.first_source_id.to_numpy(),g.first_source_id) and np.array_equal(source.last_source_id.to_numpy(),g.last_source_id)
  close(source.sample_span_ms+source.median_step_ms,g.native_duration_ms)
  for condition,rows in g.groupby('representation'):
   for r in rows.itertuples():
    geom=cnngroups['original',r.sub,r.Trial_Id] if condition=='original' else groups[condition,r.sub,r.Trial_Id]
    positions=geom[(geom.time_start>=r.start_ms-1e-7)&(geom.time_start<=r.last_sample_ms+1e-7)]
    assert len(positions)==r.n_native_samples
    assert (positions.source_row_id.iloc[0],positions.source_row_id.iloc[-1])==(r.first_source_id,r.last_source_id)
reference_groups={(s,t,event,ref,condition):g.sort_values('start_ms') for (s,t,event,ref,condition),g in events[events.model=='GT'].groupby(['sub','Trial_Id','event','reference_definition','representation'])}
for (s,t) in keys:
 g=groups['original',s,t];y=truth.iloc[g.source_row_id.to_numpy()].ground_truth.to_numpy()
 for event in ['fixation','saccade']:
  for ref in ['historical','gap_aware']:
   bins=defaultdict(list);group_id=0;tainted=False;previous=None
   for j in range(len(g)):
    known=not pd.isna(y[j]);target=known and y[j]==event
    if ref=='historical':
     if not known or (j>0 and pd.isna(y[j-1])):tainted=True
     if not tainted and j>0 and target!=previous:group_id+=1
     label='propagated_NA' if tainted else group_id
    else:
     if j>0 and (target!=previous or g.component_id.iloc[j]!=g.component_id.iloc[j-1]):group_id+=1
     label=group_id
    if target:bins[label].append(j)
    previous=target
   expected=sorted(bins.values(),key=lambda pos:pos[0])
   for condition in ['original','rate30']:
    e=reference_groups.get((s,t,event,ref,condition),events.iloc[:0])
    assert len(e)==len(expected)
    for r,pos in zip(e.itertuples(),expected):
     close([r.start_ms,r.last_sample_ms,r.native_duration_ms],[g.time_start.iloc[pos[0]],g.time_start.iloc[pos[-1]],g.time_start.iloc[pos[-1]]-g.time_start.iloc[pos[0]]])
     assert (r.first_source_id,r.last_source_id,r.n_native_samples)==(g.source_row_id.iloc[pos[0]],g.source_row_id.iloc[pos[-1]],len(pos))
print('All native source records, sampled reference runs and gap-aware boundaries PASS.',flush=True)

kcols=['model','representation','reference_definition','event','sub','Trial_Id']
counts=events.groupby(kcols).size()
status_cache={p:read(p) for p in pd.read_csv(root/'output/part2b/event_input_readiness.csv').status_path.unique()}
ready=pd.read_csv(root/'output/part2b/event_input_readiness.csv').fillna('')
for r in ready.itertuples():
 model=r.algorithm if r.algorithm!='CNN-BLSTM' else 'CNN_'+r.branch
 st=status_cache[r.status_path]
 st=st[st.branch==r.branch] if model.startswith('CNN') else st[st.configuration_id==r.configuration_id]
 actual=roster[(roster.model==model)&(roster.representation==r.representation)&(roster.event==r.event)].set_index(['sub','Trial_Id']).loc[keys]
 st=st.set_index(['sub','Trial_Id']).loc[keys]
 assert np.array_equal(actual.status,st.status)
 assert np.array_equal(actual.available,st.status.isin(['success','success_empty']))
 if model=='IHMM':assert (st.seed_id==0).all()
 if model.startswith('CNN_'):
  geometry=cnngroups if r.representation=='original' else groups
  assert np.array_equal(st.n_native,[len(geometry[r.representation,s,t]) for s,t in keys])
for variant,column in [('native','native_duration_ms'),('span','span_ms')]:
 calc=events.groupby(kcols)[column].agg(['size','mean','std'])
 actual=summary[summary.duration_variant==variant].set_index(kcols)
 assert len(actual)==len(roster) and actual.index.is_unique
 joined=actual.join(calc)
 expected_n=joined['size'].fillna(0).where(joined.available)
 close(joined.N,expected_n);close(joined.Mean,joined['mean']);close(joined.SD,joined['std'])
 assert joined.loc[joined.N==0,['Mean','SD']].isna().all().all() and joined.loc[joined.N==1,'SD'].isna().all()
 assert np.array_equal(joined.complete_N_Mean_SD,joined.available & joined[['N','Mean','SD']].notna().all(axis=1))
 assert not events.merge(roster,on=kcols,validate='many_to_one').available.eq(False).any()
distribution=read(out/'event_distribution_summary.csv')
for r in distribution.itertuples():
 q=summary[(summary.model==r.model)&(summary.representation==r.representation)&(summary.reference_definition==r.reference_definition)&(summary.event==r.event)&(summary.duration_variant==r.duration_variant)]
 for measure in ['N','Mean','SD']:
  assert getattr(r,measure+'_denominator')==q[measure].notna().sum()
 close([r.trial_N_mean,r.trial_N_sample_SD,r.trial_Mean_duration_mean_ms,r.trial_Mean_duration_sample_SD_ms,r.trial_SD_duration_mean_ms,r.trial_SD_duration_sample_SD_ms],
       [q.N.mean(),q.N.std(),q.Mean.mean(),q.Mean.std(),q.SD.mean(),q.SD.std()])
 e=events[(events.model==r.model)&(events.representation==r.representation)&(events.reference_definition==r.reference_definition)&(events.event==r.event)]
 v=e['native_duration_ms' if r.duration_variant=='native' else 'span_ms']
 assert r.pooled_events==len(v);close([r.pooled_event_duration_mean_ms,r.pooled_event_duration_sample_SD_ms],[v.mean(),v.std()])
print('Trial summaries, zero/singleton/failure handling and pooled-versus-trial distributions PASS.',flush=True)

# Independent historical smoother and derivative implementation (no detector calls).
settings=read(root/'output/part2b/final_fixed_configurations.csv');speedcache={}
cal=np.array([.01865554,.01919689])
def compatible(model,condition,s,t):
 ck=(model,condition,s,t)
 if ck in speedcache:return speedcache[ck]
 g=groups[condition,s,t];xy=g[['x','y']].to_numpy().copy();n=len(g)
 setting=settings[(settings.algorithm==model)&(settings.representation==condition)].preprocessing.iloc[0]
 lo=np.arange(n);hi=lo.copy()
 if n>=5:
  if setting in ['Mean3','Median3']:
   smooth=np.full_like(xy,np.nan)
   for j in range(1,n-1):smooth[j]=np.mean(xy[j-1:j+2],axis=0) if setting=='Mean3' else np.median(xy[j-1:j+2],axis=0)
   xy=smooth;lo=np.maximum(0,lo-1);hi=np.minimum(n-1,hi+1)
  elif setting=='SG5_23' and n>=23:
   xy=savgol_filter(xy,23,5,axis=0,mode='interp');lo=np.maximum(0,lo-11);hi=np.minimum(n-1,hi+11)
   lo[:11]=0;hi[:11]=22;lo[-11:]=n-23;hi[-11:]=n-1
 time=g.time_start.to_numpy()/1000;velocity=np.full(n,np.nan);unsafe=np.zeros(n,bool)
 if model=='EK03':
  for j in range(2,n-2):
   z=xy*cal;v=(z[j+2]+z[j+1]-z[j-1]-z[j-2])/(1.5*(time[j+2]-time[j-2]));velocity[j]=np.linalg.norm(v)
   deps=[j-2,j-1,j+1,j+2];a=lo[deps].min();b=hi[deps].max();unsafe[j]=g.component_id.iloc[a]!=g.component_id.iloc[b]
 else:
  velocity[1:]=np.linalg.norm(np.diff(xy*cal,axis=0),axis=1)/np.diff(time)
  for j in range(1,n):
   a=min(lo[j-1],lo[j]);b=max(hi[j-1],hi[j]);unsafe[j]=g.component_id.iloc[a]!=g.component_id.iloc[b]
 speedcache[ck]=(velocity,unsafe);return velocity,unsafe
vel=read(out/'saccade_velocity_events.csv');eventmap=events.set_index('event_id');assert not vel.duplicated(['event_id','velocity_rule']).any()
for r in vel.itertuples():
 e=eventmap.loc[r.event_id];g=groups[r.representation,r.sub,r.Trial_Id]
 if r.model.startswith('CNN_') and r.representation=='original':g=cnngroups[r.representation,r.sub,r.Trial_Id]
 positions=np.flatnonzero((g.time_start.to_numpy()>=e.start_ms-1e-7)&(g.time_start.to_numpy()<=e.last_sample_ms+1e-7))
 if r.velocity_rule=='harmonised_raw_within_event':
  if len(positions)<2:values=np.empty(0);bad=0;reason='insufficient_within_event_pairs'
  else:
   z=g.iloc[positions];values=np.linalg.norm(np.diff(z[['x','y']].to_numpy()*cal,axis=0),axis=1)/np.diff(z.time_start.to_numpy()/1000)
   bad=int((np.diff(z.component_id)!=0).sum());reason='blocked_interval' if bad else 'invalid_coordinate_or_dt' if not np.isfinite(values).all() else 'defined'
 else:
  v,b=compatible(r.model,r.representation,r.sub,r.Trial_Id);values=v[positions];bad=int(b[positions].sum())
  reason='blocked_derivative_or_smoothing_support' if bad else 'no_finite_historical_speed' if not np.isfinite(values).any() else 'defined'
 finite=values[np.isfinite(values)]
 expected=[finite.mean(),finite.max()] if reason=='defined' else [np.nan,np.nan]
 close(expected,[r.average_velocity_deg_s,r.peak_velocity_deg_s],atol=2e-5,rtol=1e-7)
 assert r.reason==reason and r.n_derivatives==len(values) and r.n_finite_derivatives==len(finite) and r.blocked_derivatives==bad
assert np.isclose(np.linalg.norm(np.array([3.,4.]))/1.,5.)
print('All harmonised velocities, historical preprocessing/derivatives and blocked-support reasons PASS.',len(vel),flush=True)
if pretransport_only:
 preserve()
 (out/'validation/pretransport_independent_verification.json').write_text(json.dumps(dict(passed=True,native_events=len(events),trial_summary_rows=len(summary),velocity_rows=len(vel),pinned_sources=len(registry)),indent=2))
 sys.exit(0)

# Independent uniform-mass OT: exact assignment on Euclidean costs, not the R solver.
hist=read(out/'wasserstein_historical_compatibility.csv');harm=read(out/'wasserstein_harmonised.csv')
ledger=read(out/'complete_case_ledger.csv');scaling=read(out/'reference_scaling.csv').set_index('comparison_id');solver_cache={}
primary=['IVT','IDT','EK03','IHMM','CNN_raw'];models=primary+['CNN_mean','CNN_median','CNN_sg_p3_n7','CNN_sg_p5_n23']
blocks={}
def getblock(ref,variant,event):
 ck=(ref,variant,event)
 if ck not in blocks:
  s=summary[(summary.event==event)&(summary.duration_variant==variant)]
  def get(model,condition):
   q=s[(s.model==model)&(s.representation==condition)&(s.reference_definition==(ref if model=='GT' else 'native'))]
   return q.set_index(['sub','Trial_Id']).loc[keys]
  gt=get('GT','original');choices={(m,c):get(m,c) for m in models for c in ['original','rate30']}
  common=gt.complete_N_Mean_SD.to_numpy().copy();available=gt.available.to_numpy().copy()
  for m in primary:
   for c in ['original','rate30']:
    common &= choices[m,c].complete_N_Mean_SD.to_numpy();available &= choices[m,c].available.to_numpy()
  blocks[ck]=(gt,choices,common,available)
 return blocks[ck]
def hashkeys(keep):return hashlib.sha256(','.join(np.array(keytext)[keep]).encode()).hexdigest()
distances_verified=0
for r in pd.concat([hist,harm],ignore_index=True).itertuples():
 gt,choices,common,available=getblock(r.reference_definition,r.duration_variant,r.event);d=choices[r.model,r.representation]
 if r.support_policy in ['historical_method_specific_complete','method_specific_complete_sensitivity_same_as_historical']:keep=gt.complete_N_Mean_SD.to_numpy()&d.complete_N_Mean_SD.to_numpy()
 elif r.support_policy=='primary_five_common_complete':keep=common
 elif r.support_policy=='CNN_sensitivity_paired_branch_complete':keep=common&choices[r.model,'original'].complete_N_Mean_SD.to_numpy()&choices[r.model,'rate30'].complete_N_Mean_SD.to_numpy()
 elif r.support_policy=='count_only_common_available':keep=available&choices[r.model,'original'].available.to_numpy()&choices[r.model,'rate30'].available.to_numpy()
 else:raise AssertionError(r.support_policy)
 assert keep.sum()==r.n_trials and 792-r.n_trials==r.excluded_trials and r.support_sha256==hashkeys(keep)
 actual=ledger[ledger.comparison_id==r.comparison_id].set_index(['sub','Trial_Id']).loc[keys]
 assert np.array_equal(actual.included,keep)
 if keep.sum()<3:assert r.status=='insufficient_complete_trials';continue
 g=gt.loc[keep,['N','Mean','SD']].to_numpy();a=d.loc[keep,['N','Mean','SD']].to_numpy()
 centre=np.mean(g,axis=0);scale=np.std(g,axis=0,ddof=1);scale[scale==0]=1
 if r.support_policy=='count_only_common_available':
  unit=np.mean(np.abs(np.sort(g[:,0])-np.sort(a[:,0])));close([r.distance_N,r.similarity_N,r.distance_N_native_units],[unit/scale[0],1/(1+unit/scale[0]),unit]);assert pd.isna(r.distance_multi)
 else:
  gs=(g-centre)/scale;as_=(a-centre)/scale
  digest=hashlib.sha256(gs.tobytes()+as_.tobytes()).hexdigest()
  if digest not in solver_cache:
   costs=cdist(gs,as_,metric='euclidean');ii,jj=linear_sum_assignment(costs);multi=costs[ii,jj].mean()
   solver_cache[digest]=np.r_[multi,np.mean(np.abs(np.sort(gs,axis=0)-np.sort(as_,axis=0)),axis=0)]
  distances=solver_cache[digest]
  close([r.distance_multi,r.distance_N,r.distance_Mean,r.distance_SD],distances,atol=2e-6,rtol=1e-7)
  close([r.similarity_multi,r.similarity_N,r.similarity_Mean,r.similarity_SD],1/(1+distances),atol=2e-6,rtol=1e-7)
  close([r.distance_N_native_units,r.distance_Mean_ms,r.distance_SD_ms],np.mean(np.abs(np.sort(g,axis=0)-np.sort(a,axis=0)),axis=0))
  saved=scaling.loc[r.comparison_id];close([saved.N_centre,saved.Mean_centre_ms,saved.SD_centre_ms],centre);close([saved.N_scale,saved.Mean_scale_ms,saved.SD_scale_ms],scale)
 distances_verified+=1
assert np.isclose(cdist([[0,0]],[[3,4]])[0,0],5)
fixtures=json.loads((out/'validation/transport_fixtures.json').read_text());assert fixtures['passed']
print('Independent exact-assignment Wasserstein, scaling and all support identities PASS.',distances_verified,flush=True)

obs=read(out/'figure_source_observations.csv');curves=read(out/'figure_source_curves.csv')
event_lookup=events.set_index('event_id');velocity_lookup=vel[vel.velocity_rule=='harmonised_raw_within_event'].set_index('event_id')
count_lookup=summary[summary.duration_variant=='native'].set_index(['model','representation','event','sub','Trial_Id','reference_definition']).N.to_dict()
duration_lookup=event_lookup.native_duration_ms.to_dict()
velocity_values={name:velocity_lookup[name].to_dict() for name in ['average_velocity_deg_s','peak_velocity_deg_s']}
for r in obs.itertuples():
 if r.metric=='N':
  s,t=map(int,r.observation_id.split('/'));condition='original' if r.panel.endswith('_original') else 'rate30'
  event='fixation' if '_fixation_' in r.figure else 'saccade'
  expected=count_lookup[r.series,condition,event,s,t,'historical' if r.series=='GT' else 'native']
 elif r.metric=='native_duration_ms':expected=duration_lookup[r.observation_id]
 else:expected=velocity_values[r.metric][r.observation_id]
 close(expected,r.value);assert r.included_in_curve==(np.isfinite(expected) and expected>=0)
observation_groups={k:g for k,g in obs.groupby(['figure','panel','series'])}
for (fig,panel,series),g in curves.groupby(['figure','panel','series']):
 o=observation_groups[fig,panel,series]
 assert (g.included_observations==o.included_in_curve.sum()).all() and (g.all_observations==len(o)).all()
 assert np.isfinite(g[['x','y']]).all().all() and (g.y>=-1e-12).all()
 if g.kind.iloc[0]=='PMF':
  values=o.loc[o.included_in_curve,'value'];counts=values.value_counts()
  close(g.y,[counts.get(x,0)/len(values) for x in g.x]);close(g.y.sum(),1)
 else:assert len(g)==512
assert curves.figure.nunique()==12
preserve()
gate=dict(passed=True,pinned_sources=len(registry),native_events=len(events),trial_summary_rows=len(summary),
 velocity_rows_verified=len(vel),transport_comparisons_verified=distances_verified,independent_assignment_problems=len(solver_cache),
 source_reference_and_native_events=True,zero_singleton_and_failure_rules=True,historical_and_harmonised_velocities=True,
 shared_support_and_scaling=True,figure_observations_verified=len(obs),figure_curve_points=len(curves),figures=12,
 frozen_sources_and_earlier_outputs_unchanged=True,no_detector_or_neural_calls=True)
(out/'validation/independent_verification.json').write_text(json.dumps(gate,indent=2))
print('Independent Part 3 verification PASS',json.dumps(gate),flush=True)
