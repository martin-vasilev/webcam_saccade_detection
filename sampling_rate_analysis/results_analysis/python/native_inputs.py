"""Read-only native-artifact adapter. No detection or prediction reconstruction."""
from pathlib import Path
import sys,json,hashlib
sys.dont_write_bytecode=True
import pandas as pd
import numpy as np
root=Path(sys.argv[1]).resolve();out=root/'output/part3';(out/'derived').mkdir(parents=True,exist_ok=True)
registry=pd.read_csv(root/'config/part3/input_manifest.csv');paths=dict(zip(registry.id,registry.path))
def read(id):return pd.read_csv(paths[id],float_precision='round_trip')
truth=read('truth');truth['source_row_id']=np.arange(len(truth))
trials=read('trials');keys=list(zip(trials['sub'],trials.Trial_Id))
valid=truth[(truth.conf>0)&(truth.el_pupil>0)].sort_values(['sub','Trial_Id','time_start']).copy()
edge=read('blocked_edges');edges={(s,t):g.set_index(['left_source_row_id','right_source_row_id']) for (s,t),g in edge.groupby(['sub','Trial_Id'])}
original={};rate30={};cnn_original={};coordinates=[];cnn_coordinates=[];periods=[]
for key,g in valid.groupby(['sub','Trial_Id'],sort=False):
 if key not in set(keys):continue
 g=g.copy();ids=g.source_row_id.to_numpy();t=g.time_start.to_numpy()
 assert (np.diff(t)>0).all() and np.isfinite(g[['x','y','time_start']]).all().all()
 look=edges[key].loc[list(zip(ids[:-1],ids[1:]))]
 assert len(look)==len(g)-1
 component=np.r_[0,np.cumsum(~look.eligible.to_numpy())]
 g['component_id']=component;g['native_position']=np.arange(len(g));g['representation']='original';original[key]=g
 name=f'coordinates_sub{int(key[0])}_trial{int(key[1])}';r=read(name)
 locator=dict(zip(ids,component));r['component_id']=r.source_row_id.map(locator)
 assert r.component_id.notna().all();r['component_id']=r.component_id.astype(int)
 r['native_position']=np.arange(len(r));r['representation']='rate30';rate30[key]=r
 fields=['representation','sub','Trial_Id','source_row_id','native_position','time_start','x','y','component_id']
 coordinates.extend([g[fields],r[fields]])
 full=truth[(truth['sub']==key[0])&(truth.Trial_Id==key[1])].sort_values('time_start').copy()
 full['component_id']=full.source_row_id.map(locator)
 invalid=full.component_id.isna();full.loc[invalid,'component_id']=-full.loc[invalid,'source_row_id']-1
 full['component_id']=full.component_id.astype(int);full['native_position']=np.arange(len(full));full['representation']='original'
 cnn_original[key]=full;cnn_coordinates.append(full[fields])
 periods.append(dict(sub=key[0],Trial_Id=key[1],original_n=len(g),rate30_n=len(r),
  cnn_original_n=len(full),cnn_original_extra_filtered_rows=int(invalid.sum()),cnn_original_start_ms=full.time_start.iloc[0],cnn_original_last_ms=full.time_start.iloc[-1],
  original_start_ms=t[0],original_last_ms=t[-1],rate30_start_ms=r.time_start.iloc[0],rate30_last_ms=r.time_start.iloc[-1],
  rate30_leading_omission_ms=r.time_start.iloc[0]-t[0],rate30_trailing_omission_ms=t[-1]-r.time_start.iloc[-1]))
assert set(original)==set(rate30)==set(keys)
pd.concat(coordinates,ignore_index=True).to_csv(out/'derived/coordinates.csv',index=False)
pd.concat(cnn_coordinates,ignore_index=True).to_csv(out/'derived/cnn_original_coordinates.csv',index=False)
pd.DataFrame(periods).to_csv(out/'derived/trial_recording_periods.csv',index=False)
events=[];roster=[]
def event_row(model,condition,ref,event,key,start,last,duration,first,lastid,n,ordinal,cfg,source,record):
 return dict(model=model,representation=condition,reference_definition=ref,event=event,sub=int(key[0]),Trial_Id=int(key[1]),
  event_id=f'{model}:{condition}:{ref}:{event}:{int(key[0])}:{int(key[1])}:{ordinal}',
  configuration_id=cfg,start_ms=float(start),last_sample_ms=float(last),native_duration_ms=float(duration),span_ms=float(last-start),
  first_source_id=int(first),last_source_id=int(lastid),n_native_samples=int(n),source_path=source,source_record=int(record))
for key,g in original.items():
 for target in ['fixation','saccade']:
  b=g.ground_truth.to_numpy()==target
  for ref in ['historical','gap_aware']:
   change=np.r_[False,b[1:]!=b[:-1]].astype(float)
   if ref=='historical':
    unknown=g.ground_truth.isna().to_numpy();change[unknown|np.r_[False,unknown[:-1]]]=np.nan
    group=np.cumsum(change);group=np.where(np.isnan(group),-1,group)
   else:
    change[1:]=np.logical_or(change[1:],np.diff(g.component_id)!=0)
    group=np.cumsum(change)
   for groupid in np.unique(group[b]):
    pos=np.flatnonzero((group==groupid)&b);a=g.iloc[pos[0]];z=g.iloc[pos[-1]]
    for condition in ['original','rate30']:
     events.append(event_row('GT',condition,ref,target,key,a.time_start,z.time_start,z.time_start-a.time_start,
      a.source_row_id,z.source_row_id,len(pos),int(groupid),'sampled_manual_reference',paths['truth'],int(a.source_row_id)))
   for condition in ['original','rate30']:
    roster.append(dict(model='GT',representation=condition,reference_definition=ref,event=target,sub=key[0],Trial_Id=key[1],
      configuration_id='sampled_manual_reference',status='reference_available',available=True))

ready=pd.read_csv(root/'output/part2b/event_input_readiness.csv').fillna('');cache={}
for path in set(ready.event_path)|set(ready.status_path):cache[path]=pd.read_csv(path,float_precision='round_trip')
for r in ready.itertuples():
 model=r.algorithm if r.algorithm!='CNN-BLSTM' else 'CNN_'+r.branch
 e=cache[r.event_path];s=cache[r.status_path]
 if r.algorithm=='CNN-BLSTM':
  e=e[(e.branch==r.branch)&(e.label==(1 if r.event=='fixation' else 2))];s=s[s.branch==r.branch]
 else:
  e=e[e.configuration_id==r.configuration_id];s=s[s.configuration_id==r.configuration_id]
 assert len(s)==792 and not s.duplicated(['sub','Trial_Id']).any()
 for st in s.itertuples():
  available=st.status in ['success','success_empty']
  roster.append(dict(model=model,representation=r.representation,reference_definition='native',event=r.event,
   sub=st.sub,Trial_Id=st.Trial_Id,configuration_id=r.configuration_id,status=st.status,available=available))
 ordinals={}
 for index,z in e.iterrows():
  key=(int(z['sub']),int(z.Trial_Id));g=(cnn_original[key] if r.algorithm=='CNN-BLSTM' else original[key]) if r.representation=='original' else rate30[key]
  end=z.last_sample_ms if r.algorithm=='CNN-BLSTM' else z.end_ms
  start=z.start_ms;subset=g[(g.time_start>=start-1e-7)&(g.time_start<=end+1e-7)]
  assert len(subset)>0 and abs(subset.time_start.iloc[0]-start)<1e-7 and abs(subset.time_start.iloc[-1]-end)<1e-7,(model,r.representation,key,start,end)
  if r.algorithm=='CNN-BLSTM':
   assert len(subset)==z.n_samples and subset.source_row_id.iloc[0]==z.first_source_id and subset.source_row_id.iloc[-1]==z.last_source_id
   duration=z.duration_ms
  else:duration=z.duration
  ordinals[key]=ordinals.get(key,0)+1
  events.append(event_row(model,r.representation,'native',r.event,key,start,end,duration,subset.source_row_id.iloc[0],
   subset.source_row_id.iloc[-1],len(subset),ordinals[key],r.configuration_id,r.event_path,index))
ev=pd.DataFrame(events);st=pd.DataFrame(roster)
assert ev.event_id.is_unique and not st.duplicated(['model','representation','reference_definition','event','sub','Trial_Id']).any()
ev.to_csv(out/'derived/native_events.csv',index=False);st.to_csv(out/'derived/call_roster.csv',index=False)
print('Native events and reference/coordinate identities prepared:',len(ev),'events;',len(st),'explicit trial-event statuses.',flush=True)
