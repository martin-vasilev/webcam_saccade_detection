"""Reuse passed prechecks only under exact artifact/code/version hashes.

The unchanged full verifier remains available and is the clean-run fallback.
New transport and figure checks always run from that same tested implementation.
"""
from pathlib import Path
import hashlib,json,os,sys,shutil,subprocess
sys.dont_write_bytecode=True
import numpy as np
import pandas as pd
import scipy
from scipy.spatial.distance import cdist
from scipy.optimize import linear_sum_assignment
root=Path(sys.argv[1]).resolve();out=root/'output/part3';full=root/'validation/part3/verify_full_v1.py'
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
cache_path=out/'validation/pretransport_cache.json'
valid=False
if cache_path.exists() and '--pretransport' not in sys.argv[2:]:
 try:
  cache=json.loads(cache_path.read_text())
  valid=cache['passed'] and all(sha(p)==s for p,s in cache['files'].items())
  environment=json.loads((out/'validation/python_environment_pretransport.json').read_text())
  valid=valid and environment['python']==sys.version and environment['numpy']==np.__version__ and environment['pandas']==pd.__version__ and environment['scipy']==scipy.__version__
 except (OSError,KeyError,ValueError):valid=False
if not valid:
 print('No unchanged preverification cache: executing complete independent verifier.',flush=True)
 subprocess.run([sys.executable,'-B',str(full),str(root),*sys.argv[2:]],check=True)
 if '--pretransport' not in sys.argv[2:]:subprocess.run([sys.executable,'-B',str(root/'validation/part3/verify_figure_support.py'),str(root)],check=True)
 sys.exit(0)
print('Reusing passed native/reference/summary/velocity checks: exact stage, source-manifest, verifier-code and environment hashes unchanged.',flush=True)
registry=pd.read_csv(root/'config/part3/input_manifest.csv');paths=dict(zip(registry.id,registry.path))
def preserve():
 for r in registry.itertuples():assert sha(r.path)==r.sha256,r.id
def read(p):return pd.read_csv(p,float_precision='round_trip')
def close(a,b,atol=1e-7,rtol=1e-9):np.testing.assert_allclose(np.asarray(a,dtype=float),np.asarray(b,dtype=float),atol=atol,rtol=rtol,equal_nan=True)
preserve()
shutil.copyfile(out/'validation/python_environment_pretransport.json',out/'validation/python_environment.json')
events=read(out/'derived/native_events.csv');summary=read(out/'event_trial_summary.csv');vel=read(out/'saccade_velocity_events.csv')
trials=read(paths['trials']);keys=list(zip(trials['sub'],trials.Trial_Id));keytext=[f'{s}/{t}' for s,t in keys]
source=full.read_text(encoding='utf-8');marker='# Independent uniform-mass OT:'
assert source.count(marker)==1
exec(compile(source[source.index(marker):],str(full)+' [post-transport portion]', 'exec'),globals())
subprocess.run([sys.executable,'-B',str(root/'validation/part3/verify_figure_support.py'),str(root)],check=True)
record=json.loads((out/'validation/independent_verification.json').read_text())
record.update(pretransport_checks_reused_under_exact_hashes=True,tested_full_verifier_sha256=sha(full),pretransport_cache_sha256=sha(cache_path))
(out/'validation/independent_verification.json').write_text(json.dumps(record,indent=2))
print('Fresh transport/figure validation and hash-guarded prior prechecks jointly PASS.',flush=True)
