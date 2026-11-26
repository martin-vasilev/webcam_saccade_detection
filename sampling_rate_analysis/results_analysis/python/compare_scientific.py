"""Compare genuinely recomputed science against frozen references; never produces scientific estimates."""
from pathlib import Path
import sys,json
import pandas as pd,numpy as np
package,run=map(Path,sys.argv[1:3]);ref=package/'data/frozen';checks=[]
def read(p):return pd.read_csv(p,float_precision='round_trip')
def compare(actual,expected,keys,columns=None,subset=False,label=''):
 a=read(actual);b=read(expected)
 if keys:
  assert not a.duplicated(keys).any() and not b.duplicated(keys).any(),label
  if subset:b=b.set_index(keys).loc[pd.MultiIndex.from_frame(a[keys])].reset_index() if len(keys)>1 else b.set_index(keys).loc[a[keys[0]]].reset_index()
  else:
   a=a.sort_values(keys).reset_index(drop=True);b=b.sort_values(keys).reset_index(drop=True)
 assert len(a)==len(b),(label,len(a),len(b))
 cols=columns or [c for c in a if c in b]
 for c in cols:
  x,y=a[c],b[c]
  if pd.api.types.is_numeric_dtype(x) and pd.api.types.is_numeric_dtype(y):np.testing.assert_allclose(x,y,atol=2e-6 if 'wasserstein' in label else 1e-9,rtol=1e-9,equal_nan=True,err_msg=label+':'+c)
  else:assert np.array_equal(x.fillna('<NA>').astype(str),y.fillna('<NA>').astype(str)),(label,c)
 checks.append(dict(product=label,rows=len(a),columns=len(cols),passed=True))

if (run/'derived_counts/part1_landscapes.csv').exists():
 data=read(run/'derived_counts/part1_landscapes.csv')
 for method,file in [('classical','grid_source.csv'),('hmm','hmm_source.csv')]:
  a=data[data.method.eq('IHMM') if method=='hmm' else data.method.ne('IHMM')]
  tmp=run/'validation'/('comparison_'+method+'.csv');a.to_csv(tmp,index=False)
  compare(tmp,ref/'part1'/file,['condition','configuration_id'],['MCC','scored_rows','TP','TN','FP','FN'],label='part1 '+method+' full grid')
original=json.loads((run/'input_paths.json').read_text())
for family in ['b','h','cnn']:
 f=run/f'derived_counts/{family}_counts.csv.gz'
 if family=='cnn':f=run/'derived_counts/cnn_counts.csv'
 if not f.exists():continue
 b=read(original[family+'_counts']);a=read(f)
 if family!='cnn':
  b=b[b.condition.isin(['original','rate30'])];keys=['condition','configuration_id','sub','Trial_Id'];cols=['status','common_rows','TP','TN','FP','FN']
 else:b=b[b.scope.eq('cnn_primary')];keys=['condition','event','algorithm','sub'];cols=['n','TP','TN','FP','FN','MCC']
 expected=run/'validation'/f'expected_{family}_counts.csv';b.to_csv(expected,index=False)
 compare(f,expected,keys,cols,label=family+' prediction/event-level reconstructed counts')

for name in ['heldout_performance','cnn_five_branch_performance']:
 p=run/f'output/part2/tables/{name}.csv'
 if p.exists():compare(p,ref/f'part2/{name}.csv',['algorithm','representation','event','selection_policy']+(['evaluation_support'] if name=='heldout_performance' else ['branch']),label='part2 '+name)
p=run/'output/part2/tables/selected_configurations.csv'
if p.exists():
 a=read(p);assert len(a)==160 and a.heldout_sub.nunique()==20
 for _,g in a.groupby(['condition','method']):assert g.heldout_sub.nunique()==20
 tmp=run/'validation/selection_without_ID.csv';a.drop(columns='heldout_sub').to_csv(tmp,index=False)
 compare(tmp,ref/'part2/selection_diagnostics_anonymised.csv',None,label='part2 all frozen selections/training identities')
for name in ['final_fixed_configurations','configuration_stability','performance_interpretation']:
 p=run/f'output/part2b/{name}.csv'
 if p.exists():compare(p,ref/f'part2b/{name}.csv',None,label='part2b '+name)
for name in ['event_distribution_summary','saccade_velocity_summary','saccade_velocity_missingness','wasserstein_harmonised','wasserstein_historical_compatibility','reference_scaling']:
 p=run/f'output/part3/{name}.csv'
 if not p.exists():continue
 keys={'event_distribution_summary':['model','representation','reference_definition','event','duration_variant'],
       'saccade_velocity_summary':['model','representation','reference_definition','velocity_rule'],
       'saccade_velocity_missingness':['model','representation','reference_definition','velocity_rule','reason']}.get(name,['comparison_id'])
 compare(p,ref/f'part3/{name}.csv',keys,subset=name.startswith('wasserstein') or name=='reference_scaling',label='part3 '+name)
for name in ['figure_source_curves','duration_convention_sensitivity','primary_transport_with_sensitivity_endpoints','representation_distribution_contrasts','native_input_exposure_summary']:
 p=run/f'output/part3/{name}.csv'
 if p.exists():compare(p,ref/f'part3/{name}.csv',None,label='part3 full '+name)
assert checks,'No genuinely recomputed scientific products present; refusing an empty PASS'
status=dict(passed=True,scope='Only products present in this private scientific run; not a declaration that all stages/full transport were executed',checks=checks)
(run/'validation/frozen_comparison.json').write_text(json.dumps(status,indent=2)+'\n',encoding='utf-8');print(json.dumps(status,indent=2))
