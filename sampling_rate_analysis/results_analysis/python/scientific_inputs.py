"""Explicit external input resolver. No scientific aggregate fallback or implicit workspace lookup."""
from pathlib import Path
import csv,json,hashlib,sys
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for b in iter(lambda:f.read(8*1024*1024),b''):h.update(b)
 return h.hexdigest()
def resolve(package,input_root,run):
 import pandas as pd
 records=list(csv.DictReader((package/'config/scientific_inputs.csv').open(encoding='utf-8-sig',newline='')))
 paths={};consumed=[];seen={}
 for r in records:
  packaged=r.get('runtime_location','external').startswith('package/')
  p=((package/r['runtime_location'].removeprefix('package/')) if packaged else (input_root/r['source_relative'])).resolve()
  if not p.is_relative_to(package if packaged else input_root):raise ValueError('Input escapes configured root: '+r['id'])
  if not p.is_file():raise FileNotFoundError(f"Missing input {r['id']}: supply {r['source_relative']} under --input-root (see INPUT_DATA.md)")
  actual=seen.setdefault(str(p),None)
  if actual is None:actual=sha(p);seen[str(p)]=actual
  if actual!=r['sha256']:raise ValueError('Pinned input hash mismatch: '+r['source_relative'])
  if r['id'] in paths:assert paths[r['id']]==str(p)
  paths[r['id']]=str(p);consumed.append(dict(id=r['id'],path=str(p),sha256=actual))
 for part in ['part2','part3']:
  dest=run/'config'/part;dest.mkdir(parents=True,exist_ok=True)
  pd.DataFrame([dict(id=k,path=v,sha256=seen[v]) for k,v in paths.items()]).to_csv(dest/'input_manifest.csv',index=False)
 (run/'input_paths.json').write_text(json.dumps(paths,indent=2),encoding='utf-8')
 (run/'consumed_inputs.json').write_text(json.dumps(consumed,indent=2),encoding='utf-8')
 # Event-pointer metadata is an input to native import, not a summary-score substitute.
 r=next(x for x in records if x['id']=='event_readiness');ready=pd.read_csv(paths['event_readiness'])
 for col in ['event_path','status_path']:
  def relocate(s):
   s=str(s).replace('\\','/');marker='sampling_rate_analysis/'
   if marker not in s:raise ValueError('Unrecognised native pointer: '+s)
   p=input_root/(marker+s.split(marker,1)[1]);assert str(p.resolve()) in seen
   return str(p.resolve())
  ready[col]=ready[col].map(relocate)
 target=run/'output/part2b';target.mkdir(parents=True,exist_ok=True)
 ready.to_csv(target/'event_input_readiness.csv',index=False)
 return paths

if __name__=='__main__':
 package,input_root,run=map(lambda s:Path(s).resolve(),sys.argv[1:4]);run.mkdir(parents=True,exist_ok=True)
 resolve(package,input_root,run);print('External input hashes verified; no aggregate scientific output imported.')
