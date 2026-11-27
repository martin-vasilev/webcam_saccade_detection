"""Read-only checkpoint and complete-comparison coverage audit after recovery."""
from pathlib import Path
import sys,json,hashlib,itertools
sys.dont_write_bytecode=True
import pandas as pd
root=Path(sys.argv[1]).resolve();out=root/'output/part3'
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
registry=pd.read_csv(root/'config/part3/input_manifest.csv')
for r in registry.itertuples():assert sha(r.path)==r.sha256,r.id
for stage,module in [('native_input','validation/part3/prepare_inputs.py'),('summary','R/part3/events.R'),('velocity','R/part3/velocities.R'),('transport','R/part3/transport.R')]:
 g=json.loads((out/f'validation/{stage}_gate.json').read_text())
 assert sha(root/module)==g['implementation'],stage
 for rel,s in g['outputs'].items():assert sha(out/rel)==s,(stage,rel)
models=['IVT','IDT','EK03','IHMM','CNN_raw','CNN_mean','CNN_median','CNN_sg_p3_n7','CNN_sg_p5_n23']
columns=['reference_definition','duration_variant','event','model','representation','support_policy']
hist=pd.read_csv(out/'wasserstein_historical_compatibility.csv');harm=pd.read_csv(out/'wasserstein_harmonised.csv')
expected=set(itertools.product(['historical','gap_aware'],['native','span'],['fixation','saccade'],models,['original','rate30']))
assert len(hist)==144 and not hist.duplicated(columns).any()
assert set(map(tuple,hist[columns[:-1]].to_numpy()))==expected
assert set(hist.support_policy)=={'historical_method_specific_complete'}
eh=set()
for ref,variant,event,model,rate in expected:
 eh.add((ref,variant,event,model,rate,'method_specific_complete_sensitivity_same_as_historical'))
 eh.add((ref,variant,event,model,rate,'primary_five_common_complete' if model in models[:5] else 'CNN_sensitivity_paired_branch_complete'))
 if variant=='native':eh.add((ref,variant,event,model,rate,'count_only_common_available'))
assert len(harm)==360 and not harm.duplicated(columns).any()
assert set(map(tuple,harm[columns].to_numpy()))==eh
ledger=pd.read_csv(out/'complete_case_ledger.csv');scaling=pd.read_csv(out/'reference_scaling.csv')
assert len(ledger)==360*792 and ledger.comparison_id.nunique()==360
assert (ledger.groupby('comparison_id').size()==792).all()
assert not ledger.duplicated(['comparison_id','sub','Trial_Id']).any()
assert len(scaling)==scaling.comparison_id.nunique()==360
gate=dict(passed=True,sources_verified=len(registry),producer_gates_verified=['native_input','summary','velocity','transport'],
 historical_comparisons=144,harmonised_and_sensitivity_comparisons=360,trial_support_records=len(ledger),
 complete_comparison_cartesian_coverage=True,exact_transport_outputs_reused=True)
(out/'validation/recovered_state_audit.json').write_text(json.dumps(gate,indent=2))
print('Recovered checkpoints, full comparison coverage and immutable sources PASS',json.dumps(gate))
