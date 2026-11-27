"""Independently verify every plotted observation, exposure identity and PMF."""
from pathlib import Path
import sys,json,hashlib
sys.dont_write_bytecode=True
import pandas as pd
import numpy as np
root=Path(sys.argv[1]).resolve();out=root/'output/part3'
destination=out/'validation/figure_staging' if '--staging' in sys.argv[2:] else out
def read(p):return pd.read_csv(p,float_precision='round_trip')
def close(a,b):np.testing.assert_allclose(np.asarray(a,dtype=float),np.asarray(b,dtype=float),atol=1e-7,rtol=1e-9,equal_nan=True)
s=read(out/'event_trial_summary.csv');s=s[s.duration_variant=='native']
e=read(out/'derived/native_events.csv');v=read(out/'saccade_velocity_events.csv');v=v[v.velocity_rule=='harmonised_raw_within_event'].set_index('event_id')
obs=read(destination/'figure_source_observations.csv');curves=read(destination/'figure_source_curves.csv')
trials=read(root.parents[1]/'sampling_rate_analysis/analysis_b/runs/b_001/config/trials.csv')
keys=list(zip(trials['sub'],trials.Trial_Id));keytext=np.array([f'{a}/{b}' for a,b in keys])
summaries={k:g.set_index(['sub','Trial_Id']).loc[keys] for k,g in s.groupby(['model','representation','reference_definition','event'])}
e['trial_key']=[f'{a}/{b}' for a,b in zip(e['sub'],e.Trial_Id)]
eventgroups={k:g.set_index('event_id') for k,g in e.groupby(['model','representation','reference_definition','event'])}
supports={}
for event in ['fixation','saccade']:
 gt=summaries['GT','original','historical',event];complete=gt.complete_N_Mean_SD.to_numpy().copy();available=gt.available.to_numpy().copy()
 for model in ['IVT','IDT','EK03','IHMM','CNN_raw']:
  for condition in ['original','rate30']:
   q=summaries[model,condition,'native',event];complete&=q.complete_N_Mean_SD.to_numpy();available&=q.available.to_numpy()
 supports[event,'primary_five_common_complete']=complete
 supports[event,'count_only_common_available']=available
groups={k:g for k,g in obs.groupby(['figure','panel','series'])}
for (figure,panel,series),o in groups.items():
 condition='original' if panel.endswith('_original') else 'rate30';event='fixation' if '_fixation_' in figure else 'saccade'
 metric=o.metric.iloc[0];policy='primary_five_common_complete' if metric=='native_duration_ms' else 'count_only_common_available'
 keep=supports[event,policy];allowed=set(keytext[keep]);ref='historical' if series=='GT' else 'native'
 assert (o.support_policy==policy).all() and (o.exposure_trials==keep.sum()).all()
 assert o.observation_id.is_unique
 if metric=='N':
  q=summaries[series,condition,ref,event].loc[keep];expected=pd.Series(q.N.to_numpy(),index=keytext[keep])
 else:
  q=eventgroups[series,condition,ref,event];q=q[q.trial_key.isin(allowed)]
  expected=q.native_duration_ms if metric=='native_duration_ms' else v.loc[q.index,metric]
 assert set(o.observation_id)==set(expected.index),(figure,panel,series)
 values=expected.loc[o.observation_id].to_numpy();close(o.value,values)
 assert np.array_equal(o.included_in_curve,np.isfinite(values)&(values>=0))
for key,g in curves.groupby(['figure','panel','series']):
 o=groups[key];assert (g.included_observations==o.included_in_curve.sum()).all() and (g.all_observations==len(o)).all()
 assert np.isfinite(g[['x','y']]).all().all() and (g.y>=-1e-12).all()
 if g.kind.iloc[0]=='PMF':
  values=o.loc[o.included_in_curve,'value'];counts=values.value_counts();close(g.y,[counts.get(x,0)/len(values) for x in g.x]);close(g.y.sum(),1)
 else:assert len(g)==512
assert curves.figure.nunique()==12 and len(groups)==240
report=dict(passed=True,figures=12,series_panels=len(groups),observations=len(obs),curve_points=len(curves),
 exact_frozen_trial_supports=True,all_expected_native_event_or_trial_ids=True,plotted_values_verified=True,discrete_PMFS_verified=True,
 source_observations_sha256=hashlib.sha256((destination/'figure_source_observations.csv').read_bytes()).hexdigest(),
 source_curves_sha256=hashlib.sha256((destination/'figure_source_curves.csv').read_bytes()).hexdigest())
(destination/'figure_source_verification.json').write_text(json.dumps(report,indent=2))
print('Independent figure sources, exact exposure identities and PMFs PASS',json.dumps(report),flush=True)
