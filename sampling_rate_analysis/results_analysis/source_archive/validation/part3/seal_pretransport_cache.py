"""Seal already passed, unchanged pre-transport checks; no new scientific result."""
from pathlib import Path
import hashlib,json
root=Path(__file__).resolve().parents[2];out=root/'output/part3'
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
proof=out/'validation/pretransport_independent_verification.json'
assert json.loads(proof.read_text())['passed']
assert sha(root/'validation/part3/verify.py')==sha(root/'validation/part3/verify_full_v1.py')
files={}
for stage in ['native_input','summary','velocity']:
 p=out/f'validation/{stage}_gate.json';gate=json.loads(p.read_text());files[str(p)]=sha(p)
 for rel,s in gate['outputs'].items():
  p=out/rel;assert sha(p)==s,(stage,rel);files[str(p)]=s
for p in [proof,out/'validation/python_environment_pretransport.json',root/'config/part3/input_manifest.csv',root/'config/part3/event_protocol.json',root/'validation/part3/verify_full_v1.py']:
 files[str(p)]=sha(p)
cache=dict(passed=True,scope='independent native/reference/summary/velocity checks only; new transport and figures still require validation',
 tested_implementation='validation/part3/verify_full_v1.py',immutable_stage_outputs_match_preexisting_R_checkpoints=True,files=files)
(out/'validation/pretransport_cache.json').write_text(json.dumps(cache,indent=2))
print('Previously passed pre-transport checks sealed against unchanged stage hashes.')
