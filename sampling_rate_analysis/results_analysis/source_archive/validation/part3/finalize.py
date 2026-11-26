"""Finalise only after independent numerical and explicit visual review gates."""
from pathlib import Path
import json,hashlib,sys,shutil
sys.dont_write_bytecode=True
import numpy as np
import pandas as pd
assert sys.argv[1:]==['--visual-pass']
root=Path(__file__).resolve().parents[2];out=root/'output/part3'
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
gate=json.loads((out/'validation/independent_verification.json').read_text());assert gate['passed']
assert json.loads((out/'validation/export_checks.json').read_text())['passed']
visual=json.loads((out/'validation/visual_review.json').read_text());assert visual['passed'] and len(visual['figures'])==12
for r in visual['figures']:
 assert sha(out/'figures'/(r['figure']+'.pdf'))==r['pdf_sha256']
 assert sha(out/'figures'/(r['figure']+'.png'))==r['png_sha256']
assert json.loads((out/'figure_source_verification.json').read_text())['passed']
assert json.loads((out/'validation/recovered_state_audit.json').read_text())['passed']
assert sha(out/'PART3_METHODS_DECISIONS.md')==sha(root/'config/part3/PART3_METHODS_DECISIONS.md')
shutil.copyfile(root/'config/part3/input_manifest.csv',out/'source_manifest.csv')
shutil.copyfile(root/'config/part3/event_protocol.json',out/'event_protocol.json')
(out/'logs').mkdir(exist_ok=True)
for p in (root/'logs').glob('part3*.log'):shutil.copyfile(p,out/'logs'/p.name)
registry=pd.read_csv(root/'config/part3/input_manifest.csv')
for r in registry.itertuples():assert sha(r.path)==r.sha256,r.id
transport=pd.read_csv(out/'wasserstein_harmonised.csv')
primary=transport[transport.support_policy=='primary_five_common_complete'].copy()
primary['descriptive_distance_rank']=primary.groupby(['reference_definition','duration_variant','event','representation']).distance_multi.rank(method='min')
primary.to_csv(out/'primary_transport_with_sensitivity_endpoints.csv',index=False)
native=primary[primary.duration_variant=='native'];span=primary[primary.duration_variant=='span']
comparison=native.merge(span,on=['model','representation','event','reference_definition'],suffixes=('_native','_span'))
comparison['distance_difference_span_minus_native']=comparison.distance_multi_span-comparison.distance_multi_native
comparison['descriptive_rank_changed']=comparison.descriptive_distance_rank_native!=comparison.descriptive_distance_rank_span
comparison.to_csv(out/'duration_convention_sensitivity.csv',index=False)
direction=[]
for (model,event,ref,variant),g in primary.groupby(['model','event','reference_definition','duration_variant']):
 assert len(g)==2 and g.support_sha256.nunique()==1
 o=g[g.representation=='original'].iloc[0];r=g[g.representation=='rate30'].iloc[0]
 direction.append(dict(model=model,event=event,reference_definition=ref,duration_variant=variant,n_trials=o.n_trials,
  support_sha256=o.support_sha256,distance_original=o.distance_multi,distance_rate30=r.distance_multi,
  distance_difference_rate30_minus_original=r.distance_multi-o.distance_multi,
  descriptive_direction='30_Hz_distribution_closer' if r.distance_multi<o.distance_multi else 'Original_distribution_closer' if o.distance_multi<r.distance_multi else 'equal'))
direction=pd.DataFrame(direction);direction.to_csv(out/'representation_distribution_contrasts.csv',index=False)
dn=direction[direction.duration_variant=='native'];ds=direction[direction.duration_variant=='span']
dc=dn.merge(ds,on=['model','event','reference_definition'],suffixes=('_native','_span'))
flips=int((dc.descriptive_direction_native!=dc.descriptive_direction_span).sum())
reference=pd.read_csv(out/'reference_event_validation.csv')
ref_unique=reference.drop_duplicates(['sub','Trial_Id'])
affected=ref_unique[ref_unique.missing_reference_labels>0]
count_change=reference[reference.N_historical!=reference.N_gap_aware]
velocity=pd.read_csv(out/'saccade_velocity_summary.csv')
reasons=pd.read_csv(out/'saccade_velocity_missingness.csv')
periods=pd.read_csv(out/'derived/trial_recording_periods.csv')
exposure=pd.DataFrame([
 dict(representation='Original',role='classical_detector_and_sampled_reference_input',source_samples=int(periods.original_n.sum()),policy='conf>0 and el_pupil>0'),
 dict(representation='Original',role='CNN_full_native_sequence',source_samples=int(periods.cnn_original_n.sum()),policy='frozen C3-B full sequence; native events retained'),
 dict(representation='30 Hz',role='classical_and_CNN_observed_frame_input',source_samples=int(periods.rate30_n.sum()),policy='archived observed-frame representation'),
 dict(representation='30 Hz',role='reference_event_construction_source',source_samples=int(periods.original_n.sum()),policy='Original sampled reference boundaries; velocity measured on 30-Hz frames')])
exposure.to_csv(out/'native_input_exposure_summary.csv',index=False)
def md(df):
 f=df.copy()
 for c in f:
  if f[c].dtype.kind=='f':f[c]=f[c].map(lambda x:'NA' if pd.isna(x) else f'{x:.4f}')
 return '| '+' | '.join(f.columns)+' |\n| '+' | '.join(['---']*len(f.columns))+' |\n'+'\n'.join('| '+' | '.join(map(str,r))+' |' for r in f.itertuples(index=False,name=None))
display=native[native.reference_definition=='historical'][['model','representation','event','n_trials','distance_multi','similarity_multi','distance_N','distance_Mean','distance_SD']]
support=primary.groupby(['reference_definition','duration_variant','event']).agg(n_trials=('n_trials','first'),support_identities=('support_sha256','nunique')).reset_index()
assert (support.support_identities==1).all()
gate.update(visual_review=True,export_checks=True,phase3_complete=True,
 legacy_reference_NA_labels=int(ref_unique.missing_reference_labels.sum()),legacy_reference_NA_trials=len(affected))
(out/'validation/completion_gate.json').write_text(json.dumps(gate,indent=2))
text='''# Phase 3 — COMPLETE / PASS, with explicit reference and exposure limitations

## Scope and fixed protocol

The frozen event protocol is `config/part3/event_protocol.json` (v1.0.2). All exact Phase 2B classical settings, primary HMM seed schedule 0 and CNN Raw primary/four sensitivity branches were preserved. No detector, HMM fit, neural training/inference, new parameter selection or manuscript/Git operation was performed. All scientific files consumed and all preceding result bundles remain unchanged. Part 4 is unstarted.

The R entry point is `Rscript run_results.R --stage part3`. It imports native artifacts, validates the historical reference source, derives summaries/velocities, runs the pinned R transport function, writes figures and invokes an independent Python verifier. Intermediate modules have hash-checked stage gates; a completed Part 3 bundle is verified and reused. The existing part1, part2 and part2b entry points verify/reuse their original manifests through byte-identical archived dispatchers. Session/package versions, exact input/output hashes and verification records are included.

## Historical-reference finding that affects interpretation

'''+f'''There are **{int(ref_unique.missing_reference_labels.sum())} unknown reference labels in {len(affected)} of the 792 valid-input trials**. The literal historical R code propagates an NA transition through `cumsum`; later target samples can share an NA event ID even across intervening non-target runs. Its event grouping was reproduced exactly and independently checked. Thus the historical primary is a **compatibility endpoint with a confirmed reference-construction defect**, not an independently adjudicated event segmentation. This finding was made during input validation, before event tables or rankings; the initial v1.0 protocol was retained and the NA semantics clarified as v1.0.1 before calculations.

The gap-aware sensitivity breaks at unknown labels and at frozen invalid/blink/75-ms-cap edges, without altering native detector events. {len(count_change)} participant-trial/event records have different reference counts between these definitions. This difference can reflect both gap handling and NA propagation, and is not attributed solely to sampling gaps. `reference_event_validation.csv` identifies every affected trial and recording-period boundary. Unknown labels were neither filled nor converted into new human annotations. No EyeLink-clock manual interval was substituted.

'''+'''## Native input exposure

Corrected C3-B Original CNN uses the full original sequence, including confidence/pupil-invalid rows; classical Original and historical reference use filtered rows. This documented production policy was confirmed at an endpoint-mapping gate before any event table or ranking. Protocol v1.0.2 records a separate full-native CNN coordinate map, and every source anchor/sample count was verified. Native runs were not cropped or replaced. The complete-case comparison aligns trial identities, **not identical within-trial sample exposure**. Leading/trailing input periods and extra filtered rows are retained in `derived/trial_recording_periods.csv`. Invalid-support CNN velocities are unavailable rather than estimated by row removal or imputation. This difference is an intrinsic limitation of comparing these existing native exports.

## Common-support event-distribution results

Each row of a transport input is a participant-trial vector [N, Mean duration, sample SD duration]. Uniform optimal transport compares the empirical distributions and can move mass between trials; it is not an event-matching or within-trial error score. The main table uses the same complete trial identities for all five primary families and both rates, with one reference centre/sample-SD scale per event/reference definition. The following are native-duration compatibility-reference results; distances are **raw transport costs on standardized features**, not unstandardized mixed-unit costs. Similarity is separately 1/(1+d).

'''+md(display)+'''

Common complete-trial supports:

'''+md(support)+'''

`wasserstein_historical_compatibility.csv` retains the exact original wrapper's method-specific complete cases and reference scaling. `wasserstein_harmonised.csv` contains primary common-support, paired CNN sensitivity, count-only and explicitly labelled method-specific sensitivity rows. Univariate standardized distances/similarities and native-unit N/Mean/SD distances are separate fields. The count-only common-success comparison includes successful zero-event trials. The complete 792-trial roster, failures, singleton SD=NA and zero-event Mean/SD=NA remain in separate availability/exclusion ledgers.

## Duration/reference sensitivity and limits of conclusions

'''+f'''Native classical duration is end minus start; CNN native duration is last minus first plus its trial median step. Span sensitivity uses first-to-last samples for all models and does not reinterpret the final timestamp as exclusive occupancy. Across both reference definitions and rates, {int(comparison.descriptive_rank_changed.sum())} of {len(comparison)} model/event/rate rows change descriptive distance rank between native and span conventions. {flips} of {len(dc)} model/event/reference cross-rate descriptive directions change between native and span. These are endpoint-sensitivity descriptions, not hypothesis tests or a basis for choosing configurations.

'''+'''Exact distances, support hashes and rank/direction changes are saved in `duration_convention_sensitivity.csv`, `primary_transport_with_sensitivity_endpoints.csv` and `representation_distribution_contrasts.csv`. Gap-aware comparisons also change reference distributions, complete-trial composition and reference scaling; their absolute scores must not be interpreted as a controlled improvement over the historical-reference scores. Conclusions should be stated conditional on the declared reference and duration convention. No winning algorithm/branch or population-level superiority is inferred.

The historical NA-merging defect is scientifically material even if a descriptive ordering happens to persist. For later manuscript integration, Martin should decide whether the gap-aware/unknown-breaking construction should become a separately versioned primary reference. This task retains the requested literal historical primary and presents its limitations, rather than silently replacing it.

## Saccade velocity

Harmonised velocities use raw observed webcam x/y at each representation, historical project degree-per-pixel constants, actual elapsed time and successive pairs wholly within native event boundaries. The same reference event intervals are measured on each representation's available observed coordinates. These are reference-event velocities on webcam coordinates, **not EyeLink vendor/physiological ground-truth velocities**. The 30-Hz peak has coarser measurement resolution. No interpolation, subframe peak or missing coordinate was invented.

Historical-compatible classical velocities use the exact saved-input preprocessing and pinned derivative/boundary rules. Any derivative or smoothing stencil crossing a frozen blocked interval makes that event estimate unavailable with a reason; supported estimates retain the original mean/max and finite-value handling. Harmonised within-event velocities can also be undefined for singleton/insufficient-support or blocked events. No partial across-gap estimate is silently substituted. Full event-level values, counts and reasons are retained.

Velocity densities condition on event-specific finite, supported measurements. The retained event subsets and denominators can differ across methods and rates even on common trial exposure. These are not matched-event causal comparisons; the missingness ledgers are essential context for interpreting each curve.

'''+md(velocity[velocity.velocity_rule=='harmonised_raw_within_event'][['model','representation','reference_definition','events','defined_events','undefined_events','mean_event_average_velocity_deg_s','mean_event_peak_velocity_deg_s']])+'''

## Figures and validation

Six primary plus six all-five-CNN-sensitivity PNG/vector-PDF pairs display fixation/saccade native durations, discrete trial event counts and saccade mean/peak harmonised velocities. Durations/velocities show densities in explicitly labelled log10(1+x) coordinates, retaining the full finite range. Counts use empirical discrete PMFs. Duration panels share primary common-complete trial exposure; velocity/count panels share primary common-success exposure. Finite/all event counts and exposure identities are recorded. The source observations and every saved curve point are available for audit; densities are event-pooled and therefore weight high-event trials more heavily.

'''+f'''Independent verification passed for {gate['native_events']} canonical native/reference event records, {gate['trial_summary_rows']} trial-summary rows, {gate['velocity_rows_verified']} velocity rows and {gate['transport_comparisons_verified']} transport comparisons. {gate['independent_assignment_problems']} distinct standardized Euclidean OT problems were solved independently using exact uniform-mass assignment, alongside sorted univariate W1, identity/translation fixtures, reference scaling and all support hashes. Figure source observations/PMFs and curve ledgers were checked. All 12 final PDF renders were visually inspected; PDFs are vector and PNGs are 300 dpi. Earlier complete outputs and {gate['pinned_sources']} pinned scientific/dependency records were rehashed unchanged.

'''+'''Readiness for later work does not remove the declared limitations: development-selected classical settings; provisional post-descriptive CNN branch choice; different native duration conventions; conditional complete-trial distributions; reference NA/gap semantics; measurement-resolution-dependent velocity; historical calibration assumptions; and unsupported velocity estimates. Rate contrasts combine rate-specific frozen classical settings and separately trained CNN models with Option A's unequal optimisation budgets and native input policies; they do not isolate a causal hardware-frequency effect. No new inferential intervals or tests were run. Review the reference-construction decision before manuscript integration or Part 4. No required supported calculation remains unfinished; unavailable individual estimates are explicit NA with source-supported reasons.
'''
text += '''
## Completed output inventory

- `PART3_METHODS_DECISIONS.md`, `event_protocol.json`, `source_manifest.csv`: frozen definitions and source identities.
- `derived/native_events.csv`, `derived/call_roster.csv`, coordinate maps and recording periods: native/reference identities and full trial rosters.
- `reference_event_validation.csv`: literal historical R match, NA-label counts and gap-aware differences.
- `event_trial_summary.csv`, `event_distribution_summary.csv`, `event_availability_and_exclusions.csv`: N, Mean, sample SD, pooled-event summaries and explicit denominators/statuses.
- `wasserstein_historical_compatibility.csv`: all 144 historical-compatible comparisons.
- `wasserstein_harmonised.csv`: all 360 common-support, count-only, CNN-branch and method-specific sensitivity comparisons.
- `complete_case_ledger.csv`, `reference_scaling.csv`: 285,120 comparison/trial records and 360 support/scaling records.
- `saccade_velocity_events.csv`, `saccade_velocity_summary.csv`, `saccade_velocity_missingness.csv`: all reconstructed velocities and NA reasons.
- `duration_convention_sensitivity.csv`, `representation_distribution_contrasts.csv`, `primary_transport_with_sensitivity_endpoints.csv`: descriptive endpoint/rate contrasts.
- `figures/`: twelve final 300-dpi PNG/vector-PDF pairs and captions. `figure_source_observations.csv` and `figure_source_curves.csv` contain their scientific values.
- `validation/`: producer checkpoints, independent numerical/source/exposure/export gates, session/package records, fixtures and final completion gate. `validation/figure_staging/` is retained diagnostic work, not the final figure directory.
- `output_manifest.json`: final scientific output and implementation hashes; execution/diagnostic logs are archived in `logs/`.

## Recovery and reproduction

Recovery on 2026-11-22 found all eight exact transport groups already committed. Their hashes and full comparison coverage passed, so **no exact R optimal-transport calculation was rerun**. The interrupted verifier lacked two SciPy imports; this implementation issue was corrected without changing distances or scientific rules. The independently passed native/reference/summary/124,837-velocity checks were reused only after matching stage/output, tested-code and environment hashes. Exact independent assignment calculations and all remaining figure checks then ran. All twelve final figures were reviewed after a presentation-only redraw; the observation/curve table hashes remained unchanged.

From the extension directory, using the recorded R installation:

```powershell
& '<EXTERNAL_ROOT>' run_results.R --stage part1
& '<EXTERNAL_ROOT>' run_results.R --stage part2
& '<EXTERNAL_ROOT>' run_results.R --stage part2b
& '<EXTERNAL_ROOT>' run_results.R --stage part3
```

Completed runs verify/reuse their sealed outputs. The Part 3 entry point checks intermediate gates before calculation, so valid exact-transport outputs are reused in an interrupted pre-finalisation run as well. A missing/mismatched preverification cache invokes the full independent verifier. For a separately versioned clean reproduction, retain the pinned sources/configuration, run the Part 3 entry point, review every resulting figure, run `validation/part3/inspect_exports.py` with the bundled PDF Python and only then run `validation/part3/finalize.py --visual-pass` with the project scientific Python. Do not delete or overwrite this completed bundle to force a fresh run.

Remaining decisions for Martin are scientific, not unfinished execution: whether a separately versioned unknown-breaking/gap-aware reference should replace the defective literal historical construction for future primary reporting; how to address Original CNN's wider native exposure in any additional boundary-censoring sensitivity; and how to interpret resolution-dependent webcam-coordinate velocities. None is silently resolved by choosing favourable distribution scores. No new event calculation, detector, neural inference/training or linguistic analysis is required to inspect this completed bundle.
'''
(out/'PHASE3_STATUS.md').write_text(text,encoding='utf-8')
for r in registry.itertuples():assert sha(r.path)==r.sha256,r.id
files={str(p.relative_to(out)).replace('\\','/'):sha(p) for p in sorted(out.rglob('*')) if p.is_file() and p.name!='output_manifest.json'}
implementation=[root/'run_results.R',*sorted((root/'R/part3').glob('*.R')),*sorted((root/'validation/part3').glob('*.py')),*sorted((root/'validation/part3').glob('*.R')),*sorted((root/'config/part3').rglob('*'))]
implementation=[p for p in implementation if p.is_file()]
manifest=dict(passed=True,stage='part3',outputs=files,implementation={str(p.relative_to(root)).replace('\\','/'):sha(p) for p in implementation},
 source_manifest_sha256=sha(root/'config/part3/input_manifest.csv'),protocol_sha256=sha(root/'config/part3/event_protocol.json'))
(out/'output_manifest.json').write_text(json.dumps(manifest,indent=2))
print('Part 3 final completion, scientific preservation and output manifest PASS.')
