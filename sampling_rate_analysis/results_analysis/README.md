# Webcam eye-event detection: scientific Results pipeline

One R entry point orchestrates the validated R and Python implementations for Results Parts 1Ã¢â‚¬â€œ3. It compares I-VT, I-DT, EK03, I-HMM and corrected CNN-BLSTM at Original webcam timestamps (approximately 55 Hz) and observed-frame 30 Hz. Scientific outputs, external participant data and provisional public review materials have separate locations.

The tables and figures in `output/review/` are provisional review materials derived from validated scientific analyses. Final manuscript presentation will be determined following discussion with the research team.

## Three execution modes

Install the dependencies in `environment/DEPENDENCIES.md`. Python is required for scientific recomputation, not merely for plotting. Use an authorised external frozen-input bundle described in [INPUT_DATA.md](INPUT_DATA.md). No participant data or model weights are bundled.

From this directory, the real-data validation route is:

```bash
Rscript run_results.R --stage all --mode recompute --input-root "INPUT_BUNDLE" --output-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE" --detector-scope pilot --transport-scope representative
```

Replace the capitalised placeholders with paths. The scientific run computes full-grid sample scoring, CNN prediction scoring, training-only LOPO selection, full-development fixed settings, native/reference events and velocities. `pilot` re-executes historical classical preprocessing/detection for two real trials at each rate, all 320 configurations (1,280 calls); every full-grid event is independently rescored. `representative` solves prespecified real-data exact-transport comparisons on their full frozen trial supports, not a trial subsample. These execution scopes are **not a claim that every detector and all 504 transport comparisons were rerun**. Exact measured coverage and independent frozen matches are in `validation/SCIENTIFIC_PIPELINE_VALIDATION.md`.

The implementation also has `--detector-scope full` and `--transport-scope full` paths. They are not yet certified as tested full-production commands. Their execution coverage is explicitly recorded separately; do not infer it from representative tests. Use a new private run directory for a changed execution scope. `--detector-scope archived` explicitly skips detector execution and starts from saved native outputs. CNN training/inference and stochastic I-HMM refitting are never performed here.

Verify computed products, then regenerate their review presentation:

```bash
Rscript run_results.R --stage all --mode verify --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part1 --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part2 --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part2b --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
```

Part 3 review from a new run requires **full** transport/curve products; representative transport will fail closed rather than borrow frozen curves. The complete previously validated Part 3 review bundle remains available in the public candidate.

The existing aggregate reuse routes remain:

```bash
Rscript run_results.R --stage all --mode verify
Rscript run_results.R --stage all --mode review
python python/verify_release.py
```

Without `--scientific-root`, these check/regenerate the bundled frozen aggregates, **not raw scientific recomputation**. Stage selectors are `all`, `part1`, `part2`, `part2b`, `part3`. A standalone selection stage may consume explicitly registered per-trial confusion ledgers; an all-stage run supplies newly reconstructed counts. Stage 2B requires the preceding newly computed LOPO output, and Part 3 requires newly computed fixed configurations in the same private run. Missing or changed inputs produce explicit errors; there is no fallback to final aggregate scores.

## What the calculations mean

Part 1 classical grids are exploratory whole-cohort landscapes on configuration-specific three-rate paired Original-reference support. I-HMM has its own per-trial fitting mechanism; saved seed-0 events are scored without refitting. CNN has five independently trained held-out branches, not a selector chosen using their held-out scores.

Part 2 uses 20 participant-held-out folds. Classical selection first maximises available training rows, then averages fixation/saccade training MCC on aligned training support; it shares preprocessing but permits event-specific thresholds. Part 2B applies the same frozen criterion to the full development cohort. Its development scores are in-sample and are not LOPO policy performance. CNN Raw remains the provisional primary branch, with four fixed sensitivities.

Part 3 retains native event boundaries, historical and gap-aware reference definitions, zero/singleton/failure rules, duration sensitivities, exact standardised Wasserstein distances and historical/harmonised velocities. Historical NA/cumsum behaviour, Original CNN's wider input exposure and support differences remain explicit. Distributional transport is not matched-event accuracy. Sample-based resampling comparisons do not isolate a hardware-frequency causal effect; Original settings were developed on this cohort and CNN Option A has unequal optimisation budgets.

## Find the outputs and code

- `R/executable/`, `python/`: real scientific implementations, input/scoring adapters and independent checks; code lineage is in `config/executable_code_manifest.json`.
- [ANALYSIS_FLOW.md](ANALYSIS_FLOW.md): each scientific product and review figure mapped to its producer and inputs.
- Configured private output root: full-precision participant/trial/event products, execution logs, checkpoints, scientific review regeneration and independent validation. **Do not add this directory to Git.**
- `output/review/`: preserved compact public-review tables/PNG/vector-PDF figures; `data/frozen/` and `output/supplementary/`: full-precision aggregate supplements and sensitivities.
- [METHODS.md](METHODS.md), [REPRODUCIBILITY.md](REPRODUCIBILITY.md), [PUBLIC_RELEASE_AUDIT.md](PUBLIC_RELEASE_AUDIT.md): scientific definitions, measured reproducibility boundaries and publication clearance.

Staged against main `2d2b7d5827c481f3b82296cc5be1f1d2e5a89ef9`. Original science/manuscripts and the existing review outputs remain frozen. A local release commit is prepared separately after final validation. No push, PR or manuscript edit is included. The complete release scope has author-confirmed publication permission; see PUBLIC_RELEASE_AUDIT.md. Remote publication still requires separate user approval. Actual scientific computation coverage is reported independently from publication readiness.

## Rendered files and numerical verification

The subtree's `.gitattributes` preserves exact bytes, including historical line endings, so checkout does not invalidate scientific/source SHA-256 manifests. On Windows with a long checkout path, enable local Git long-path support with `git config core.longpaths true`; this does not rename any scientific files or directories.

`python python/verify_release.py` keeps byte-hash checks strict for scientific inputs, source/configuration manifests, tables and other non-render files. For PNG/PDF outputs only, it records changed snapshot hashes and sets `render_review_required`, while still verifying full-precision metrics, supports, plot ledgers and curve sources. This numerical PASS does not certify unreviewed image changes. Inspect differences and record the rendering review before updating a release snapshot. Use `python python/verify_release.py --strict-render-bytes` to verify an unchanged assembled snapshot exactly. A reissued inventory must retain every frozen scientific hash; never refresh it to hide changed data or code. Final local release validation and rendering findings are recorded in `validation/FINAL_RELEASE_VALIDATION.md`.
