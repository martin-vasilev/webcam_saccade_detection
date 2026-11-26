"""One-time evidence pin and pre-outcome protocol freeze for Part 3."""
from pathlib import Path
import csv,hashlib,json,shutil
root=Path(__file__).resolve().parents[2];w=root.parents[1]
def sha(p):
 h=hashlib.sha256()
 with Path(p).open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
c=root/'config/part3';c.mkdir(parents=True,exist_ok=True)
assert not (c/'event_protocol.json').exists(),'Never repin a frozen protocol'
archive=root/'validation/part3/run_results_part2b_v3.R';shutil.copyfile(root/'run_results.R',archive)
rows={}
def add(id,path,expected='',role='consumed source'):
 p=Path(path).resolve();s=sha(p)
 if expected:assert s==expected,(id,p)
 if str(p) not in rows:rows[str(p)]=dict(id=id,path=str(p),sha256=s,role=role)
for r in csv.DictReader((root/'config/part2b/input_manifest.csv').open()):add(r['id'],r['path'],r['sha256'],r['role'])
m=json.loads((root/'output/part2b/output_manifest.json').read_text())
for rel,s in m['outputs'].items():add('part2b_'+rel,root/'output/part2b'/rel,s,'earlier output preservation')
for rel,s in m['implementation'].items():add('part2b_implementation_'+rel,archive if rel=='run_results.R' else root/rel,s,'earlier implementation preservation')
add('part2b_manifest',root/'output/part2b/output_manifest.json')
for name in ['original_wasserstein_functions.R','original_rmd_reference_events.R']:
 add(name,w/'classical_evaluation_reproduction/source_extracts'/name)
add('historical_get_xy',w/'classical_detection_reproduction/source_extracts/original_get_xy.R')
for name in ['blocked_edges','source_components']:
 add(name,w/f'sampling_rate_analysis/full_analysis_a_001/common/{name}.csv')
add('Smooth_source',w/'classical_detection_reproduction/package_sources/eyemovements-87d8e4465418782eb24dd910d3ca24ebb8c10a25/R/Smooth.R')
for package in ['T4transport','signal','zoo','eyemovements']:
 lib=next(p for p in [w/'classical_detection_reproduction/R_library'/package,w/'classical_evaluation_reproduction/R_library'/package] if p.exists())
 for p in sorted(lib.rglob('*')):
  if p.is_file():add('package_'+package+'_'+str(p.relative_to(lib)),p,role='pinned R dependency')
with (c/'input_manifest.csv').open('w',newline='',encoding='utf-8') as f:
 writer=csv.DictWriter(f,fieldnames=['id','path','sha256','role']);writer.writeheader();writer.writerows(rows.values())
protocol=dict(version='part3_v1.0',frozen_before_event_rankings=True,trial_roster=792,
 reference=dict(primary='historical binary target-label runs after conf>0 & el_pupil>0, sorted actual timestamps, end minus start',
 gap_sensitivity='split target runs at any ineligible edge in frozen 75-ms blocked_edges ledger (invalid/manual blink/cap); no native detector events resegmented',
 rate_policy='same Original manual-sampled reference events at both rates; no reference resampling/cropping'),
 duration=dict(primary='saved native duration: classical end-start; CNN last-first+median_step',sensitivity='first-to-last sample span for all events'),
 configurations='exact Phase 2B eight classical fixed choices; primary I-HMM seed_id 0; CNN Raw primary and four fixed sensitivity branches',
 transport=dict(package='T4transport 0.1.8',p=1,ground_metric='Euclidean, verified with 3/4 diagonal-shift fixture giving 5',masses='uniform per trial',
 historical='original compute_wasserstein_full; paired method-specific complete cases, reference sample SD scaling, zero SD=>1, n<3=>NA',
 main='common complete trials across four classical plus CNN Raw, both rates, separate event/reference definition; same reference centring/scaling for all',
 cnn_sensitivity='intersect primary-common set with that branch complete cases at both rates; reference scaling on explicitly recorded branch-specific subset',
 method_sensitivity='method-specific paired complete cases retained as historical compatibility',
 count_only='common successful-call trials across primary five families and both rates; include zero events; reference need not have 2 events',
 outputs='raw standardized transport distance and 1/(1+d) similarity, also native-unit univariate distances',
 solver_check='independent scipy linear_sum_assignment/n for equal-size uniform empirical measures; sorted differences for univariate W1'),
 velocity=dict(calibration_deg_per_pixel_x=.01865554,calibration_deg_per_pixel_y=.01919689,
 harmonised='raw observed coordinates at current representation; successive pairs wholly inside native event; arithmetic mean and maximum speed in deg/s',
 reference='same native reference boundaries, sampled only at each representation actual observed timestamps; no new interpolation',
 blocked='underlying Original blocked-edge component change forbids derivative; if any required edge/stencil is blocked, event velocity NA with reason',
 historical='exact archived whole-trial get_xy preprocessing and detector derivative; IVT/IDT previous-current, IHMM difference of degree-scaled positions, EK03 four neighbours/(1.5*(t[i+2]-t[i-2])); inclusive native endpoints',
 smoothing_support='historical derivative marked unavailable if its smoothing/derivative dependency stencil crosses blocked edge; preserve algorithm boundary/NA omission on supported samples',
 undefined='no within-event pair, missing/nonfinite positions, blocked derivative support or no finite historical speed=>explicit NA reason; no partial gap-bridged estimate',
 limitations='30-Hz peak is resolution-specific observed-frame speed; no physiological/subframe peak recovery; common calibration is historical project convention, not new participant calibration'),
 summaries=dict(zero='N=0, Mean/SD=NA on successful empty',singleton='N=1, Mean=duration, sample SD=NA',failed='all event summary measures NA, status preserved',
 distribution='separate pooled-event mean/sample SD and unweighted across-trial N/Mean/SD summaries; finite denominators explicit'),
 figures=dict(count=12,formats=['300dpi PNG','vector PDF'],duration_velocity_axes='KDE of log10(1+duration_ms) or log10(1+speed_deg_s), all finite nonnegative observations retained; transformed density labelled',
 counts='empirical discrete PMF, no KDE',duration_support='primary common-complete trials; CNN sensitivities use same exposure set with zero-event absence disclosed',velocity_support='common primary availability trials; finite-event densities with missingness retained'),
 statistical_scope='descriptive event-distribution comparisons, no population hypothesis tests or new selection',
 input_manifest_sha256=sha(c/'input_manifest.csv'))
(c/'event_protocol.json').write_text(json.dumps(protocol,indent=2),encoding='utf-8')
methods='''# Part 3 pre-outcome methods decisions

This protocol is frozen before any new event-duration, transport or velocity ranking is calculated. Authoritative machine-readable choices are in `config/part3/event_protocol.json`. No detector/refit/training/inference or parameter/branch reselection is authorised.

The primary reference reproduces the historical R binary target runs after `conf>0 & el_pupil>0`, ordered by participant/trial/actual `time_start`; duration is last minus first sample. Removed rows can bridge historical runs. The labelled sensitivity splits reference runs at every ineligible edge of the existing 75-ms invalid/blink/cap ledger. Detector-native events are never resegmented or replaced by projected label runs. The same original reference events are used at both rates; 30-Hz exposure-edge offsets are recorded rather than silently trimming events or substituting EyeLink-clock intervals.

Use the exact Phase 2B fixed configurations, primary HMM seed schedule 0 and existing saved calls. CNN Raw is now authorised as provisional primary; four other branches remain sensitivity products. CNN predictions are the existing twenty individually held-out model exports, not one new full-cohort model or a validated branch selector. The classical fixed choices use development labels, so downstream distributions are development-configured descriptions, not independent performance estimates of final model selection.

Native reported duration is primary (CNN includes its trial median-step adjustment). A second endpoint is first-to-last sample span for all models. Neither endpoint is silently interpreted as exclusive occupancy. Complete 792-trial rosters preserve successful zero events, singleton undefined sample SD and failures. Pooled event durations differ from unweighted summaries of trial-level N, Mean and SD.

Historical transport reuses the pinned R wrapper and T4transport 0.1.8: p=1, Euclidean ground cost, uniform empirical masses, joint complete cases, at least three trials, reference-centred and reference-sample-SD scaled, constant SD replaced by one. The main comparison freezes a common complete trial set across all five primary families and both rates per event/reference definition. Reference scaling is shared. Method-specific complete cases are labelled compatibility/sensitivity. Each CNN sensitivity also has an explicitly labelled paired branch-complete subset of the primary set. Count-only transport uses common successful calls and retains zero-event trials. Save standardized raw distances and their separate 1/(1+d) similarities, plus native-unit univariate distances. Transport compares empirical distributions and is not a within-trial error score.

Harmonised velocities use **raw observed** webcam coordinates for every model/reference, the historical x/y degree-per-pixel constants 0.01865554/0.01919689, actual dt and successive within-event pairs only; event mean is the arithmetic mean of supported pair speeds, peak is their maximum. At 30 Hz the same reference boundaries are measured only on 30-Hz observed frames. No extrapolation or unsupported interpolation occurs. Singleton/empty/blocked or insufficiently supported velocities are NA with explicit reasons. A 30-Hz peak is not a recovered physiological peak; the calibration is the pinned project convention, not independently established participant-specific calibration.

Classical historical-compatible velocities are reconstructed separately on the exact historical whole-trial preprocessed input. Mean/Median3 keep NA edges; SG5/23 and small-trial fallback follow the original helper. Derivative and boundary inclusion follow each pinned function, including preceding/outside-event dependencies. To honour the no-gap-bridging rule, a historical event whose derivative or smoothing stencil crosses a frozen blocked edge receives NA instead of a fabricated compatible estimate; supported events retain original finite-sample mean/max conventions. Harmonised raw velocities do not inherit smoothing stencils. Native segments may cross gaps but are not altered.

Six primary and six all-five-CNN-sensitivity figure pairs cover fixation/saccade native duration, discrete counts, and saccade mean/peak harmonised velocity. Duration/velocity plots use explicitly labelled log10(1+x) density coordinates to retain complete finite ranges; count plots use PMFs. Durations use primary common-complete trial exposure; velocity plots use common successful-call exposure and disclose finite/undefined event counts. No new inferential intervals or significance claims are made.

Validation includes exact historical R reference reconstruction, independent event/status joins and duration summaries, independent raw-coordinate velocities and historical derivative/preprocessing checks, Euclidean/identity/translation transport fixtures, independent assignment-solver distances, shared-support IDs, figure-source values and all consumed/earlier-output hashes. Any unverifiable subanalysis is blocked explicitly while independent valid products continue. Existing Parts 1/2A/2B remain byte-identical, with dispatcher compatibility adapters to archived entry points.
'''
(c/'PART3_METHODS_DECISIONS.md').write_text(methods,encoding='utf-8')
print('Protocol frozen before event calculations. Pinned sources:',len(rows))
