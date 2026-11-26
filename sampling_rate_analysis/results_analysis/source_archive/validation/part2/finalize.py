"""Finalise Part 2 after explicit visual review; preserve Part 1 and source inputs."""
from pathlib import Path
import sys
import json
import hashlib
sys.dont_write_bytecode = True
import pandas as pd

assert sys.argv[1:] == ['--visual-pass'], 'Inspect all final figures before passing --visual-pass'
root = Path(__file__).resolve().parents[2]
output = root / 'output/part2'
def sha(path): return hashlib.sha256(Path(path).read_bytes()).hexdigest()
registry = pd.read_csv(root/'config/part2/input_manifest.csv').fillna('')
for row in registry.itertuples(): assert sha(row.path)==row.sha256,row.id
gate = json.loads((output/'validation/independent_verification.json').read_text())
assert gate['passed']
assert json.loads((output/'validation/export_checks.json').read_text())['passed']
selection = pd.read_csv(output/'tables/selected_configurations.csv')
frequency = selection.groupby(['method','condition','preprocessing','fix_threshold','sacc_threshold'],dropna=False).size().reset_index(name='outer_folds')
frequency.to_csv(output/'tables/selection_frequency.csv',index=False)
scores = pd.read_csv(output/'tables/heldout_performance.csv')
shared = scores[scores.evaluation_support=='shared_policy_paired_joint']
balance=[]
for (method,condition,policy),g in shared.groupby(['algorithm','representation','selection_policy']):
    assert len(g)==2 and g.evaluation_rows.nunique()==1 and g.row_id_sha256.nunique()==1
    f=g[g.event=='fixation'].iloc[0];s=g[g.event=='saccade'].iloc[0]
    balance.append(dict(algorithm=method,representation=condition,selection_policy=policy,
        evaluation_rows=int(f.evaluation_rows),row_id_sha256=f.row_id_sha256,
        fixation_MCC=f.MCC,saccade_MCC=s.MCC,joint_descriptive_mean_MCC=f.joint_descriptive_mean_MCC,
        minimum_event_MCC=min(f.MCC,s.MCC),fixation_F1=f.F1,saccade_F1=s.F1))
balance=pd.DataFrame(balance);balance.to_csv(output/'tables/policy_balance_matched_support.csv',index=False)
new=balance[balance.selection_policy=='shared_preprocessing_joint']
old=balance[balance.selection_policy=='existing_event_specific']
comparison=new.merge(old,on=['algorithm','representation'],suffixes=('_joint','_existing'))
assert (comparison.row_id_sha256_joint==comparison.row_id_sha256_existing).all()
comparison['mean_MCC_difference']=comparison.joint_descriptive_mean_MCC_joint-comparison.joint_descriptive_mean_MCC_existing
comparison['minimum_MCC_difference']=comparison.minimum_event_MCC_joint-comparison.minimum_event_MCC_existing
comparison.to_csv(output/'tables/joint_vs_existing_balance.csv',index=False)
def md(frame):
    f=frame.copy()
    for col in f:
        if f[col].dtype.kind=='f':f[col]=f[col].map(lambda x:f'{x:.4f}')
    return '| '+' | '.join(f.columns)+' |\n| '+' | '.join(['---']*len(f.columns))+' |\n'+'\n'.join('| '+' | '.join(map(str,r))+' |' for r in f.itertuples(index=False,name=None))
joint=scores[(scores.selection_policy=='shared_preprocessing_joint')&(scores.evaluation_support=='own_policy_paired_joint')]
cnn=pd.read_csv(output/'tables/cnn_five_branch_performance.csv')
text='''# Phase 2 status — COMPLETE / PASS

## Scope and protocol

The new stage selects shared preprocessing with event-specific thresholds for I-VT, I-DT and EK03, and shared preprocessing for I-HMM. It evaluates the five existing held-out CNN branches separately. All production inputs and the completed Part 1 bundle remain unchanged. The requested main entry point is `run_results.R --stage part2`.

Candidate sufficiency was demonstrated before selection using complete 328-configuration/trial ledgers per representation, explicit success/error states, common reference-row totals and positive-class counts. Both event calls have predictions on every eligible row of a successful trial. Saved event intervals and actual original reference timestamps independently reconstruct the selected held-out counts. No detector, HMM state fit, neural training or inference was invoked.

For each outer participant, rate and method, candidates share preprocessing but may have event-specific historical thresholds. The pinned interfaces support separate fixation and saccade calls. These remain two binary tasks and are not silently reconciled into an exhaustive multiclass sequence. HMM uses the established primary seed schedule 0 and saved per-trial fits, without seed selection.

## Joint selection and support

Exclude the outer participant first. Maximise candidate-pair joint available eligible training rows. Among maximum-availability candidates, intersect their jointly available training trials and score every tied candidate on exactly those same reference rows for both tasks. J is the equal-event mean of pooled training MCC. This conservative tie-support rule was explicitly frozen before the new results, because equal row totals need not mean identical rows. Record both candidate-specific availability and the common score-support identity.

Undefined MCC for either task makes that tied candidate inadmissible. There is no fallback to a lower-availability tier. J ties within 1e-12 follow Raw, Mean3, Median3, SG5/23 then ascending fixation and saccade thresholds. HMM has no threshold search. All 160 outer fold/rate/method selections have a defined selected outcome. Training ledgers list the other 19 participants and exact aligned trial indices/row hashes.

Three evaluation supports are saved. `condition_specific_joint` requires both selected event calls to succeed at that rate. `own_policy_paired_joint` additionally requires both calls at both rates and is the primary new-policy comparison. `shared_policy_paired_joint` intersects all three policies, both rates and both event tasks within algorithm. Direct policy comparisons use this last support. Event-specific historical and B2/C1 choices themselves remain unchanged; their predictions are newly rescored on these declared masks. Original frozen B2/C1 support tables are also preserved separately in the Part 2 bundle.

Successful empty detections remain available negatives. Failed calls are unavailable and never scored as a successful empty output. Zero-reference trials remain in the 792-trial execution roster. Nonnumeric metrics are explicit NA: zero MCC denominator, zero precision/recall denominator, or zero F1 denominator. F1 is 2TP/(2TP+FP+FN), so a missed positive task with a nonzero denominator can have F1=0. Pooled metrics use pooled counts, never averaged fold MCC. Joint descriptive mean MCC is defined only when both pooled task MCCs are defined.

## New policy held-out performance

Each row has 20 held-out participants. The reported trial availability refers to joint call availability at the given rate, while evaluation rows use the paired mask stated in the table. Per-event success/failure and scored/scorable trial counts are separate columns in the full output.

'''+md(joint[['algorithm','representation','event','evaluation_rows','available_trials','failed_trials','MCC','F1','precision','recall']])+'''

## Policy balance on controlled support

The table below compares policies on identical reference rows within each algorithm, both rates and both event tasks. These are descriptive differences conditional on those supports. No population inference, policy optimisation on held-out outcomes or winning-policy selection was performed. A higher mean can coincide with a lower score on one event, so the minimum-event score is supplied as supporting context.

'''+md(balance[['algorithm','representation','selection_policy','evaluation_rows','fixation_MCC','saccade_MCC','joint_descriptive_mean_MCC','minimum_event_MCC']])+'''

Against the existing event-specific policy on matched support, the joint policy provides no observed increase in held-out mean MCC. The means are equal for I-VT Original, I-DT 30 Hz, EK03 30 Hz and both I-HMM rates. They are lower for I-VT 30 Hz, I-DT Original and EK03 Original. A shared preprocessing constraint and a different training objective do not guarantee better held-out balance. Relative to historical fixed settings, some 30-Hz tasks improve while I-HMM remains weaker. These comparisons neither establish a practically important population improvement nor justify choosing a policy winner. See joint_vs_existing_balance.csv for exact differences.

## CNN branches

All five corrected branches retain their verified outer-held-out predictions on the frozen 68,577 reference rows. Both events have 20 participants per branch. Metrics are checked against saved prediction-level binary labels and source counts. Joint descriptive MCC does not select a branch. No inner validation set was constructed from existing LOPO predictions.

'''+md(cnn[['branch','representation','event','MCC','F1','precision','recall','joint_descriptive_mean_MCC']])+'''

## Deliverables and provenance

`tables/selected_configurations.csv` is the 160-row joint selection ledger. `candidate_scores.csv` contains all considered pairs, joint availability, aligned training identity, J and deterministic selection outcomes. `training_support.csv` records participant/trial exclusions. `policy_configuration_choices.csv` supplies actual settings for every policy, fold, rate and event. `heldout_trial_counts.csv`, pooled and participant tables preserve availability and the three masks. CNN, matched-policy comparison, balance and selection-frequency tables are separate. No generic fold-specific setting string should be treated as one universal selected threshold: consult the explicit ledger/frequency table.

Four 300-dpi PNG/vector-PDF figure pairs show new-policy MCC, new-policy F1, CNN five-branch MCC/F1 and matched-support policy comparisons. Every plotted value is linked to its row in saved figure-source tables through `validation/plot_ledger.csv`. Captions state scope and supports. No new bootstrap intervals appear.

`config/part2/protocol.json` and its input manifest freeze source identities and the policy. Independent checks cover all candidate comparisons, participant exclusion, joint event identities, deterministic ties, full call/status alignment, saved-interval held-out counts, metrics, pooling, CNN predictions, support intersections and source preservation. R session information and logs remain archived. The input preservation check covers all consumed inputs and the complete Part 1 output tree, not a fresh audit of every unconsumed historical file.

The independent gate verified 160 selections, 86,880 candidate pairs, 22,700 successful saved-event call projections, 38,016 trial records, 144 pooled summaries and 120 plotted values. All four one-page PDFs are vector graphics; corresponding PNGs are 300 dpi. Final PDF renders were visually reviewed after correcting axis-title clipping. Startup locale warnings and Poppler fallback-font warnings did not prevent export or change numeric results. Initial connection and verification-field alias errors were corrected in the new extension; attempt logs are retained. No frozen scientific source required correction.

## Reproduction and Part 1 compatibility

From the extension directory:

```powershell
& '<EXTERNAL_ROOT>' run_results.R --stage part1
& '<EXTERNAL_ROOT>' run_results.R --stage part2
```

The version-2 dispatcher verifies the completed Part 1 outputs and original implementation hashes, resolving its original `run_results.R` identity to the byte-identical archived copy at `validation/part2/run_results_part1_v1.R`. The Part 1 manifest is unchanged. The current dispatcher and new helpers are pinned by the Part 2 manifest. This explicit compatibility adapter is required because Part 1 pinned its old entry point.

Completed Part 2 invocations verify/reuse outputs. A clean separately versioned copy without Part 2 output can regenerate the numeric bundle with the same command and then requires visual review. After that review, `python -B validation/part2/finalize.py --visual-pass` records final completion. Use the existing project Python runtime if needed. Preserve existing completed output directories rather than deleting them for reproduction. Prior pending attempts and logs are retained for diagnosis, while only `output/part2/` is the completed result.

## Remaining limits and next decision

Joint support conditions on successful calls and remains representation/reference-assisted. Different algorithms can retain different supports even within the same labelled evaluation category. Parameter choices are cohort/grid-limited, and some new supports differ from older B2/C1 reports. The all-five CNN branch table has no learned branch-selection estimate. The limited task is complete, with no unresolved numerical or integrity blocker. Review selection behaviour and policy balance before freezing settings for event-level work. Parts 3–4, new event/velocity/linguistic analyses, manuscript changes and Git synchronisation remain outside this implementation.
'''
(root/'PHASE2_STATUS.md').write_text(text,encoding='utf-8');(output/'PHASE2_STATUS.md').write_text(text,encoding='utf-8')
for row in registry.itertuples(): assert sha(row.path)==row.sha256,row.id
gate.update(visual_review=True,phase2_complete=True,part1_preserved=True)
(output/'validation/completion_gate.json').write_text(json.dumps(gate,indent=2))
files={str(p.relative_to(output)).replace('\\','/'):sha(p) for p in sorted(output.rglob('*')) if p.is_file() and p.name!='output_manifest.json'}
implementation=[root/'run_results.R',*sorted((root/'R/part2').glob('*.R')),*sorted((root/'validation/part2').glob('*.py')),*sorted((root/'validation/part2').glob('*.R')),root/'config/part2/protocol.json',root/'config/part2/input_manifest.csv',root/'PHASE2_STATUS.md']
manifest=dict(passed=True,stage='part2',outputs=files,implementation={str(p.relative_to(root)).replace('\\','/'):sha(p) for p in implementation},source_manifest='config/part2/input_manifest.csv',source_manifest_sha256=sha(root/'config/part2/input_manifest.csv'))
(output/'output_manifest.json').write_text(json.dumps(manifest,indent=2))
print('Part 2 completion, source preservation and output manifest PASS.')
