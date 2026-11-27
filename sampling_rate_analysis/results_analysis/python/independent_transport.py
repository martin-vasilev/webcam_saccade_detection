from pathlib import Path
import sys,json,hashlib
import numpy as np,pandas as pd
from scipy.spatial.distance import cdist
from scipy.optimize import linear_sum_assignment
root=Path(sys.argv[1]);out=root/'output/part3'
read=lambda p:pd.read_csv(p,float_precision='round_trip')
summary=read(out/'event_trial_summary.csv');trials=read(root/'config/part3/trials.csv');keys=list(zip(trials['sub'],trials.Trial_Id));keytext=[f'{s}/{t}' for s,t in keys]
def close(a,b,atol=1e-7,rtol=1e-9):np.testing.assert_allclose(np.asarray(a,dtype=float),np.asarray(b,dtype=float),atol=atol,rtol=rtol,equal_nan=True)
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


(out/'validation/independent_transport_recomputed.json').write_text(json.dumps(dict(passed=True,computed_rows=distances_verified,exact_assignment_problems=len(solver_cache))))
