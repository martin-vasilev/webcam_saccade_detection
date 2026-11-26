# Reproducibility and execution coverage

The package now contains actual executable scientific modules. The previous packaging-only documentation/validation is retained unchanged under `validation/history/release_packaging_20261125/`; it describes the earlier version, not current capability.

## Modes and boundaries

| Mode | Inputs | Computation |
|---|---|---|
| `recompute` | Explicit external input bundle and private output root | Native event/prediction scoring, frozen selectors, reference/event summaries, velocities and pinned exact transport |
| `review --scientific-root` | A private run with a passed frozen comparison | Presentation from newly computed products; Part 3 requires full corresponding products |
| `verify --scientific-root` | Private scientific products | Independent comparison with frozen references; coverage is read from run records |
| `review` without a scientific root | Bundled frozen aggregates/curves | Existing review-table/figure regeneration only |
| `verify` without a scientific root | Bundled frozen results | Existing archived numerical/hash verification only |

There is no fallback from recomputation to final aggregate scores. Per-trial confusion ledgers are explicit intermediate inputs for standalone selection; all-stage execution reconstructs these from native events and held-out prediction rows. Historical comparison-policy choices remain frozen inputs; the joint LOPO and full-development selectors actually run. CNN Raw is not reselected.

## Scientific code and scope

The orchestrator is `R/scientific_pipeline.R`; module/input/output maps are in ANALYSIS_FLOW.md and INPUT_DATA.md. `config/executable_code_manifest.json` lists original source hashes, released code hashes and adaptations. The core selectors, event summaries and velocities reuse original R function code. Part 2B's numeric block and independent verification blocks are extracted verbatim; old commit/visual-finalisation dispatchers are excluded. Exact transport changes only dependency lookup and an explicit comparison-subset guard. Newly connected event scoring preserves the prior boolean interval union plus independent inclusive sweep, with its literal original excerpt retained. Metric functions are extracted verbatim from the tested downstream Python implementation.

Detector execution and detector-result scoring are separate. `--detector-scope pilot` replays two real trials at each rate across all 320 classical configurations. `full` offers whole-cohort replay; `archived` deliberately starts from saved native outputs. Original sample-based smoothing and full-trial calls remain unchanged. I-HMM uses archived verified seed-0 native outputs without stochastic refitting; CNN uses saved participant-held-out predictions, with no training/inference.

`--transport-scope representative` executes four prespecified real-data comparisons on their complete frozen supports: historical IVT Original fixation method-specific, fixation common-complete, fixation count-only and saccade common-complete. The method-specific sensitivity row is the same numerical result explicitly labelled by the original implementation. No trial subsampling, approximate solver or changed metric is introduced. `full` removes only that computation-subset filter. Full transport/curve execution must be reported separately from the representative real-data validation.

## Commands and prerequisites

Use the README's command with explicit `--input-root`, `--output-root`, `--python`, detector and transport scopes. Stage selectors are `all`, `part1`, `part2`, `part2b`, `part3`. For Part 2B/3, use the same private output root as the preceding newly computed stages. The resolver currently requires the complete registered input bundle even for a stage-specific invocation. Missing artifact errors identify the exact source-relative path. Changed bytes stop at the hash gate.

Scientific outputs may include participant/trial/event records and must stay outside the proposed public package. A different execution scope requires a new private run root. Successful detector replay and Part 1 calculations have hash-verified reuse checkpoints. Do not overwrite completed scientific work to force agreement. Later stage dispatchers are executable but are not a general transaction-safe distributed scheduler; only the documented completed checkpoints are reused.

Review commands from a representative private run can regenerate Parts 1/2/2B. Part 3 scientific review refuses incomplete transport/curve inputs. The complete frozen public-review bundle remains available through the separate aggregate review command. Do not relabel preserved archived figures as newly recomputed figures.

## Dependencies and computational limits

The original R dependencies include dplyr, purrr, tidyr, tibble, signal, zoo, eyemovements and T4transport 0.1.8, plus jsonlite/digest and base graphics. In particular, the historical EK03 export uses unqualified `map2`, requiring attached purrr just as in the original runner. The adapter records this environment requirement rather than changing scientific expressions. Python requires NumPy, pandas and SciPy; no neural runtime is required. The portable Python distribution used locally has isolated module lookup, so the adapter explicitly adds its own script directory for its bundled metric import.

Use ordinary configured R libraries and a suitable Python executable; no development-library path is hidden in public executable code. Runtime versions are recorded in private run session/environment records. Install third-party software under its own licence. Dependencies and participant data are not vendored. A clean dependency install and non-Windows platform have not been certified by these tests.

Full scoring accounts for 519,552 Original/30-Hz configuration-trial calls (506,880 classical and 12,672 I-HMM), while the saved 1000-Hz status ledgers are used solely to retain Part 1's inherited three-rate paired geometry. Exact transport uses complete-trial vectors and can be substantially more expensive than an aggregate verification. Runtime/storage measurements and actual validated coverage belong to SCIENTIFIC_PIPELINE_VALIDATION.md, not to assumptions inferred from source availability. No bootstrap, neural training or new statistical model is run.

## Validation and publication

Independent checks operate on genuinely computed outputs: source anchors/preprocessing; configuration/trial counts; training-only folds, candidate ties and selected choices; held-out metrics; native/reference events; velocities and missingness; exact transport by an independent SciPy assignment solver; and frozen-source numerical comparisons. Numerical tolerances follow the existing independent verifiers (metric identity typically 1e-12; velocity absolute 2e-5/relative 1e-7; transport absolute 2e-6/relative 1e-7). Check measured test records before claiming a full path passed.

Author-confirmed permission for this exact release scope, including existing LOPO diagnostics and review outputs, is recorded in PUBLIC_RELEASE_AUDIT.md. Local execution and public push remain separate; push still requires approval. No participant-level products, model weights, private logs or scientific-run output roots belong in the proposed Git diff. No commit, push, PR or manuscript modification is part of this task.

The final release distinguishes scientific numerical identity from rendering byte identity. PDF timestamps and PNG rasterisation may differ without changing plotted data. The independent verifier reports such differences explicitly; it does not silently bypass non-render hashes. Separate visual/content review is required if `render_review_required` is true. Strict snapshot byte verification is available with `--strict-render-bytes`. See README and validation/FINAL_RELEASE_VALIDATION.md.
