"""Pin the existing evidence before full-development selection; no analysis."""
from pathlib import Path
import csv, hashlib, json, shutil
root = Path(__file__).resolve().parents[2]
w = root.parents[1]
def sha(p):
    h=hashlib.sha256()
    with Path(p).open('rb') as f:
        for b in iter(lambda:f.read(4194304),b''):h.update(b)
    return h.hexdigest()
directory=root/'config/part2b';directory.mkdir(parents=True,exist_ok=True)
assert not (directory/'input_manifest.csv').exists(), 'Do not repin an existing protocol'
archive=root/'validation/part2b/run_results_part2_v2.R'
shutil.copyfile(root/'run_results.R',archive)
records={}
def add(id,p,expected='',role='consumed frozen input'):
    p=Path(p).resolve();s=sha(p)
    if expected: assert s==expected,(id,p)
    if str(p) not in records:records[str(p)]=dict(id=id,path=str(p),sha256=s,archive_sha256=expected,role=role)
for row in csv.DictReader((root/'config/part2/input_manifest.csv').open()):add(row['id'],row['path'],row['sha256'])
for stage in ['part1','part2']:
    m=json.loads((root/f'output/{stage}/output_manifest.json').read_text())
    for rel,s in m['outputs'].items():add(stage+'_'+rel,root/f'output/{stage}'/rel,s,'earlier output preservation')
    add(stage+'_manifest',root/f'output/{stage}/output_manifest.json')
    for rel,s in m['implementation'].items():
        p=root/rel
        if rel=='run_results.R':p=root/'validation/part2/run_results_part1_v1.R' if stage=='part1' else archive
        add(stage+'_implementation_'+rel,p,s,'earlier implementation preservation')
for row in csv.DictReader((root/'config/input_manifest.csv').open()):
    if row['id'].startswith('manifest_'):add(row['id'],row['path'],row['sha256'])
base=w/'sampling_rate_analysis/extension_c/c3_cnn_retraining'
for condition,subdir in [('original','c3b_original'),('rate30','c3c_rate30')]:
    run=base/subdir/'run_001';m=json.loads((run/'final_manifest.json').read_text())
    artifacts=m.get('artifact_sha256',{})
    for filename in ['tables/all_native_events.csv.gz','tables/all_trial_status.csv','tables/prediction_availability.csv','validation/completion_gate.json']:
        e=artifacts.get(filename.replace('/','\\'),artifacts.get(filename,''))
        if isinstance(e,dict):e=e['sha256']
        add('cnn_'+condition+'_'+Path(filename).stem,run/filename,e)
    add('cnn_'+condition+'_worker',base/subdir/'code/worker.py')
    if condition=='rate30':add('cnn_native_rate30',run/'inputs/native_input.csv')
inputhash=json.loads((w/'sampling_rate_analysis/analysis_b/runs/b_001/config/input_hashes.json').read_text())
truth=next(p for p in inputhash if p.endswith('webdata_manual_labels.csv'))
add('truth',truth,inputhash[truth])
add('b_input_hashes',w/'sampling_rate_analysis/analysis_b/runs/b_001/config/input_hashes.json')
for p in sorted((w/'sampling_rate_analysis/full_analysis_a_001/rate30/inputs').glob('*.csv')):
    add('coordinates_'+p.stem,p,inputhash.get(str(p),''),'native observed-frame coordinate map')
for name in ['PARAMETER_SELECTION_DECISION.md','EVENT_LEVEL_ANALYSIS_PLAN.md']:
    add('planning_'+name,w/'sampling_rate_analysis/martin_results_plan_001'/name)
for name in ['run_b_grid.R','detector_adapter.R']:
    p=w/'sampling_rate_analysis/analysis_b/scripts'/name
    if p.exists():add('code_'+name,p)
for name in ['IVT','IDT','EngbertKliegl03','IHMM']:
    add('detector_source_'+name,w/f'classical_detection_reproduction/package_sources/eyemovements-87d8e4465418782eb24dd910d3ca24ebb8c10a25/R/{name}.R')
with (directory/'input_manifest.csv').open('w',newline='',encoding='utf-8') as f:
    writer=csv.DictWriter(f,fieldnames=['id','path','sha256','archive_sha256','role']);writer.writeheader();writer.writerows(records.values())
policy=dict(version='part2b_v1.0',development_participants=20,trials=792,reference_rows=68577,
 objective='equal-event mean of pooled binary fixation and saccade MCC',
 availability='maximise jointly available eligible development rows',
 aligned_support='intersection of joint-success trials across all maximum-availability pairs, before calculating either event MCC',
 undefined='inadmissible if either MCC undefined; no lower-availability fallback',
 tie='J tolerance 1e-12, then Raw/Mean3/Median3/SG5_23, ascending fixation then saccade threshold',
 hmm_seed_id=0,cnn_policy='post-hoc rankings reported; provisional common Raw primary for minimal signal transformation, all other branches as sensitivity; requires scientific confirmation; not an optimality claim',
 interpretation='Full development-data selection; scores are not held-out fixed-configuration estimates',
 input_manifest_sha256=sha(directory/'input_manifest.csv'),archived_part2_entry_sha256=sha(archive))
(directory/'protocol.json').write_text(json.dumps(policy,indent=2))
print('Pinned inputs and previous bundles:',len(records))
