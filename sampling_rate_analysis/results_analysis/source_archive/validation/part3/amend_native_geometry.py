"""Pre-results clarification of the already frozen C3 full-sequence policy."""
from pathlib import Path
import json,shutil,hashlib,csv
root=Path(__file__).resolve().parents[2];c=root/'config/part3';w=root.parents[1]
assert not (root/'output/part3/derived/native_events.csv').exists()
p=json.loads((c/'event_protocol.json').read_text());assert p['version']=='part3_v1.0.1'
for n in ['event_protocol.json','PART3_METHODS_DECISIONS.md']:shutil.copyfile(c/n,c/'history'/('v1.0.1_'+n))
records=list(csv.DictReader((c/'input_manifest.csv').open()))
extra=w/'sampling_rate_analysis/extension_c/c3_cnn_retraining/c3b_original/run_001/frozen/original_protocol.json'
assert extra.exists()
def sha(p):return hashlib.sha256(Path(p).read_bytes()).hexdigest()
if str(extra) not in {r['path'] for r in records}:records.append(dict(id='cnn_original_protocol',path=str(extra),sha256=sha(extra),role='verified full Original sequence policy'))
shutil.copyfile(c/'input_manifest.csv',c/'history/v1.0.1_input_manifest.csv')
with (c/'input_manifest.csv').open('w',newline='',encoding='utf-8') as f:
 writer=csv.DictWriter(f,fieldnames=['id','path','sha256','role']);writer.writeheader();writer.writerows(records)
p['version']='part3_v1.0.2';p['native_input_geometry']='Classical Original/reference use conf/pupil-valid rows; corrected CNN Original uses full native trial sequence, as frozen C3-B. Derive a separate full-native raw-coordinate map and validate every native source-anchor/sample-count identity. Do not crop events. Invalid-support derivatives are unavailable.'
p['input_manifest_sha256']=sha(c/'input_manifest.csv')
(c/'event_protocol.json').write_text(json.dumps(p,indent=2))
with (c/'PART3_METHODS_DECISIONS.md').open('a',encoding='utf-8') as f:
 f.write('''\n## Pre-results clarification v1.0.2: Original CNN full-sequence exposure\n\nBefore any event table/statistic was saved, an endpoint-mapping gate exposed the documented C3-B full-sequence input policy: Original CNN retains low-confidence/pupil-invalid rows, whereas classical Original and the historical reference filter them. Use a separate full-native Original CNN coordinate map from the same pinned source. Validate all event source anchors, native sample counts and trial rosters; do not infer a valid-row mapping or silently crop native runs. All five CNN branches must share this verified full-sequence identity. The common complete **trial** set does not imply identical within-trial sample exposure. Report leading/trailing exposure and sample-count differences.\n\nHarmonised velocities remain raw observed-coordinate estimates. Full-native CNN events touching invalid support cannot acquire a velocity by removing rows, zero filling, interpolation or crossing gaps; affected estimates are unavailable. On supported valid spans the underlying raw coordinate/time basis is the same as that used by the reference. The complete-input difference limits native event-distribution interpretation, and cannot be removed without defining a separate approved boundary-censor/cropping analysis. Initial protocol versions are retained. No outcome ranking informed this clarification.\n''')
print('Pre-results native CNN geometry clarified and its frozen production protocol pinned.')
