"""Document a pre-results input/legacy-code finding; preserve initial protocol."""
from pathlib import Path
import json,shutil,hashlib
root=Path(__file__).resolve().parents[2];c=root/'config/part3'
assert not (root/'output/part3/derived/native_events.csv').exists()
p=json.loads((c/'event_protocol.json').read_text());assert p['version']=='part3_v1.0'
history=c/'history';history.mkdir(exist_ok=True)
for n in ['event_protocol.json','PART3_METHODS_DECISIONS.md']:shutil.copyfile(c/n,history/('v1.0_'+n))
p['version']='part3_v1.0.1'
p['reference']['unknown_labels']='Preserve literal original R NA/cumsum propagation in historical reference; gap-aware sensitivity treats unknown labels as explicit non-target run boundaries without propagation'
p['pre_result_amendment']='302 valid-row NA labels discovered during input validation before any native/reference event table, summary, velocity or transport result was calculated. Initial protocol retained in config/part3/history.'
(c/'event_protocol.json').write_text(json.dumps(p,indent=2),encoding='utf-8')
with (c/'PART3_METHODS_DECISIONS.md').open('a',encoding='utf-8') as f:
 f.write('''\n## Pre-results clarification v1.0.1: unknown-label propagation in the literal historical source\n\nInitial import identified 302 NA labels after the historical conf/pupil filter. No event table or ranking had been generated. The original R `is_target != lag(is_target)` followed by `cumsum` propagates an unknown transition to all later event IDs within that trial. Grouping target samples by NA event ID can merge later noncontiguous target runs. The compatibility/primary historical reference must preserve this literal behaviour to match the pinned source, rather than silently filling NA or treating it as false. The result is a **historical sampled-reference compatibility endpoint with a known reference-construction defect**, not an independently adjudicated gold-standard segmentation. Native detector events remain untouched.\n\nThe separately labelled gap-aware reference sensitivity breaks at invalid/blink/cap edges and treats unknown labels as explicit non-target boundaries without cumsum propagation. This is a disclosed reconstruction departure, not a correction to frozen original data. Reference validation records affected trial identities and label counts. Main common-support results are reported for both reference definitions. Interpret event-level conclusions conditional on reference construction, and disclose if they differ. The initial protocol/methods are retained under config/part3/history; this clarification was made from source/input evidence before observing any event-level ranking.\n''')
print('Pre-results reference semantics amended; initial protocol retained.')
