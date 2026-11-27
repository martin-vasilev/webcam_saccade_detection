# Reproducibility levels, dependencies and external inputs

| Level | Supported command | What is established |
|---|---|---|
| Archived-result reuse | `Rscript run_results.R --stage all` | Frozen byte hashes, recorded numerical identities and support constants |
| Review regeneration | `Rscript run_results.R --stage all --mode review` | Tables and figures regenerated from aggregate science, not selection, density estimation or event transport |
| Independent numerical checks | `python python/verify_release.py` | Independent confusion metrics, distance transforms and source-to-review identities |
| Full scientific recomputation from events/predictions | Not exposed as a release command | Requires external inputs, permissions and a tested portable adapter; not established here |
| Detector/preprocessing regeneration | Not part of this package | Requires pinned source implementation, reference/input ledgers and execution environment |
| CNN retraining | Not part of this package | Requires approved training inputs, feature/window caches, seeds, folds, architecture/environment and compute; not rerun |

Each R stage can be run separately with `--stage part1`, `part2`, `part2b` or `part3`. `--mode review` regenerates only that stage's presentation. Verify mode writes current validation/environment records; it leaves science and review outputs unchanged. Unknown modes fail closed. Existing aggregate CSVs are immutable inputs: any hash mismatch stops the stage. Source/output manifests are checksums, not cryptographic signatures of authorship.

## Environment

Tested with R 4.5.2 and installed `jsonlite`/`digest` versions recorded in `environment/versions.csv`; actual session metadata is in `environment/sessionInfo.txt`. Rendering uses base R graphics, Cairo PNG (300 dpi) and vector PDF; minor device/font differences may change output bytes across machines. The scientific CSV bytes, numeric values and figure-source mapping should remain invariant. Dependencies are not vendored. Independent release verification uses Python standard-library CSV/math/hashlib; its version is in the validation JSON. Original scientific recomputation additionally used T4transport 0.1.8 and Python NumPy/SciPy/pandas; inspect the archived scientific validation/code for exact operations. A clean recomputation environment has not been provisioned or certified in this task.

Use a normal R library, or set `R_LIBS_USER` before launching R if you install dependencies outside the default library. No original workspace path is embedded in the executable entry point. Library installation is a user step; it is not attempted silently. Public source data are available from the [official project](https://github.com/martin-vasilev/webcam_saccade_detection), but this does not establish that all corrected derived prediction/event artifacts are public or licensed for redistribution.

## External inputs required for full recomputation

`config/external_input_catalog.json` preserves source artifact identifiers, roles and SHA-256 values where present, with absolute local paths omitted. It is an inventory, not a download manifest. Public input availability has not been asserted for unverified derived artifacts. The following must be supplied through an explicitly approved, configurable path map before wiring or claiming full recomputation:

1. Analysis B1 full configuration confusion ledgers, B2 training/held-out ledgers and explicit detector-call statuses, the 792-trial roster and original-reference common-row mask with participant/trial/source-row identities.
2. C1 primary-seed I-HMM per-trial configuration counts, execution ledgers and fitted-event exports; selection is training-only and seed schedule stays frozen.
3. Corrected C3-B/C five-branch held-out predictions, target-grid projection and downstream common-row records. Model checkpoints are unnecessary for re-evaluating saved predictions; training/inference is a separate procedure.
4. Phase 2B fixed-event inputs, immutable manual sample reference, original/native coordinates and blocked-edge/recording-period ledgers, native event/call rosters, velocity records and complete-case trial ledgers.
5. Exact transport implementation and any original helper extracted from the pinned manuscript, plus independent trial-level transport/reference scaling fixtures.

The historical code archive is an inspectable scientific implementation, not a supported runnable pipeline: it retains relative workspace assumptions and expects a registry of external inputs. Machine-specific literal paths are redacted where noted in `config/source_code_manifest.json`. The scientific helpers in `R/scientific/` are byte-identical, with source hashes. Do not invoke old workflows against arbitrary files or treat an external filename match as a hash/identity match. A future portable full-compute adapter must map each artifact ID to an approved local path, verify every hash and trial/configuration identity, run the unchanged helpers, and independently compare against these frozen outputs. This was intentionally not guessed or simulated here.

## Privacy-preserving exclusions

No participant/trial/event rows, raw coordinates, neural predictions, models or weights are bundled. LOPO selected-configuration diagnostics remove `heldout_sub`; training/support hashes and aggregate frequencies remain. Detailed original membership ledgers, candidate/trial confusion tables and observation-level validation fixtures are retained in the protected scientific workspace. Archived JSON validation records are redacted for local paths and participant-identity keys; they preserve numerical gates but are **not a complete public raw-input validation bundle**. Aggregate results/curves are staged for author review pending permission, not asserted to be freely redistributable.
