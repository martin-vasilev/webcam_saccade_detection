"""Wiring of the validated inclusive-union/sweep scoring to saved native events and predictions.

The original operation sequence is preserved in validation/interval_scoring_original_excerpt.txt.
This adapter is explicitly new wiring; it does not replace detector or selection implementations.
"""
from pathlib import Path
import json,sys
sys.path.insert(0,str(Path(__file__).resolve().parent))
import numpy as np,pandas as pd
from scientific_metrics import counts,mcc,digest_ids
root=Path(sys.argv[1]).resolve();paths=json.loads((root/('active_paths.json' if (root/'active_paths.json').exists() else 'input_paths.json')).read_text())
out=root/'derived_counts';out.mkdir(exist_ok=True)
def read(id):return pd.read_csv(paths[id],float_precision='round_trip')
trials=read('trials');mask=read('mask');mask=mask[mask.common_eligible].sort_values('source_row_id')
assert len(mask)==68577 and mask.source_row_id.is_unique
truth=read('truth');truth['source_row_id']=np.arange(len(truth));ref=truth.set_index('source_row_id').loc[mask.source_row_id]
assert np.array_equal(ref.ground_truth,mask.ground_truth) and np.array_equal(ref['sub'],mask['sub']) and np.array_equal(ref.Trial_Id,mask.Trial_Id)
assert np.allclose(ref.time_start,mask.time_start,rtol=0,atol=1e-9)
mg={k:g for k,g in mask.groupby(['sub','Trial_Id'])};trialkeys=list(trials[['sub','Trial_Id']].itertuples(index=False,name=None))
grid=read('b_grid');hgrid=read('h_grid');allrows={};land=[]
for family,defs in [('b',grid),('h',hgrid)]:
 rows=[];availability={}
 for cond in ['original','rate30','rate1000']:
  st=read(f'{family}_status_{cond}');availability[cond]={cfg:g.set_index(['sub','Trial_Id']).status.isin(['success','success_empty']) for cfg,g in st.groupby('configuration_id')}
 for cond in ['original','rate30']:
  status=read(f'{family}_status_{cond}');events=read(f'{family}_events_{cond}')
  sg={k:g.set_index('configuration_id') for k,g in status.groupby(['sub','Trial_Id'])};eg={k:g for k,g in events.groupby(['sub','Trial_Id'])}
  for ti,key in enumerate(trialkeys):
   base=mg.get(key,mask.iloc[:0]);times=base.time_start.to_numpy();labels=base.ground_truth.to_numpy();ss=sg[key]
   assert len(ss)==len(defs)
   ee=eg.get(key,events.iloc[:0]);gg={cfg:g for cfg,g in ee.groupby('configuration_id')}
   for cfg in defs.itertuples():
    r=ss.loc[cfg.configuration_id];ok=r.status in ['success','success_empty'];e=gg.get(cfg.configuration_id);p=np.zeros(len(times),bool)
    if e is not None:
     for a,b in e[['start_ms','end_ms']].itertuples(index=False,name=None):p|=(times>=a)&(times<=b)
     independent=(np.searchsorted(np.sort(e.start_ms.to_numpy()),times,side='right')-np.searchsorted(np.sort(e.end_ms.to_numpy()),times,side='left'))>0
     assert np.array_equal(p,independent)
    else:assert r.n_events==0
    y=labels==cfg.event
    if not ok:assert e is None
    co=counts(y,p) if ok else [np.nan]*4
    paired=all(bool(availability[c][cfg.configuration_id].loc[key]) for c in availability)
    row=dict(condition=cond,configuration_id=cfg.configuration_id,method=cfg.method,event=cfg.event,sub=key[0],Trial_Id=key[1],status=r.status,common_rows=len(base),available=ok,paired_success=paired,**dict(zip(['TP','TN','FP','FN'],co)))
    if family=='h':row.update(seed_id=0,smoothing=cfg.smoothing)
    rows.append(row)
   if (ti+1)%100==0:print('native scoring',family,cond,ti+1,'/792',flush=True)
 df=pd.DataFrame(rows);dest=out/f'{family}_counts.csv.gz';df.to_csv(dest,index=False);allrows[family]=df
 paths[family+'_counts']=str(dest)
 for (cond,cfgid),q in df.groupby(['condition','configuration_id'],sort=False):
  definition=defs.set_index('configuration_id').loc[cfgid];keep=q[q.paired_success];co=keep[['TP','TN','FP','FN']].sum().to_numpy()
  z=dict(condition=cond,configuration_id=cfgid,method=definition.method,event=definition.event,preprocessing=definition.preprocessing if family=='b' else {'raw':'Raw','mean':'Mean3','median':'Median3','sg':'SG5_23'}[definition.smoothing],MCC=float(mcc(co)),scored_rows=int(sum(co)),**dict(zip(['TP','TN','FP','FN'],map(int,co))))
  land.append(z)
cnn=[]
for cond in ['original','rate30']:
 for event in ['fixation','saccade']:
  d=read(f'cnn_predictions_{event}_{cond}').sort_values('source_row_id')
  assert np.array_equal(d.source_row_id,mask.source_row_id) and np.array_equal(d.truth,mask.ground_truth.eq(event))
  for col in ['sub','Trial_Id']:assert np.array_equal(d[col],mask[col])
  assert np.allclose(d.time_start,mask.time_start,rtol=0,atol=1e-9)
  for sub,g in d.groupby('sub'):
   for algorithm in ['CNN_raw','CNN_mean','CNN_median','CNN_sg_p3_n7','CNN_sg_p5_n23']:
    assert np.isin(g[algorithm].to_numpy(),[0,1,False,True]).all(),'Non-binary/missing saved CNN prediction'
    assert g[algorithm+'_available'].all()
    co=counts(g.truth.to_numpy(dtype=bool),g[algorithm].to_numpy(dtype=bool))
    cnn.append(dict(scope='cnn_primary',algorithm=algorithm,condition=cond,event=event,sub=sub,n=len(g),**dict(zip(['TP','TN','FP','FN'],map(int,co))),MCC=float(mcc(co))))
pd.DataFrame(cnn).to_csv(out/'cnn_counts.csv',index=False);paths['cnn_counts']=str(out/'cnn_counts.csv')
pd.DataFrame(land).to_csv(out/'part1_landscapes.csv',index=False)
(root/'recomputed_paths.json').write_text(json.dumps(paths,indent=2),encoding='utf-8')
print('Counts recomputed from native events and held-out prediction rows; no final aggregate score read.',flush=True)
