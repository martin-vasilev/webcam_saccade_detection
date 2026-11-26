# Webcam eye-event detection: Results review package

This separately versioned addition packages completed Results Parts 1–3 for Martin's review. It compares I-VT, I-DT, EK03, I-HMM and corrected CNN-BLSTM at Original webcam timestamps (approximately 55 Hz) and observed-frame 30 Hz. It does not change the scientific production runs or the manuscript.

The tables and figures in `output/review/` are provisional review materials derived from validated scientific analyses. Final manuscript presentation will be determined following discussion with the research team.

## Start here

Use R with `jsonlite` and `digest` installed. No Python, neural framework or transport solver is needed for the default archived-result checks or review regeneration.

```r
install.packages(c("jsonlite", "digest"))
```

From this directory:

```bash
Rscript run_results.R --stage all
Rscript run_results.R --stage all --mode review
python python/verify_release.py
```

The first command **verifies archived aggregate science**, including hashes, confusion-count metrics and distance/similarity identities. The second **regenerates provisional review tables and PNG/PDF plots from those aggregates**. The independent Python command uses only the standard library. These are not full scientific recomputations. Each R command also accepts `--stage part1`, `part2`, `part2b` or `part3`. The script locates its own directory; the working directory and input library locations are not hard-coded.

Full recomputation from participant/trial predictions, event tables and coordinates is **not wired as a release command**. Some inputs are not in the public repository and redistribution permission has not been established. See [REPRODUCIBILITY.md](REPRODUCIBILITY.md). Do not use an archived PASS record as evidence that raw-data recomputation was run on a new machine.

## What to review

- `output/review/tables/`: readable Markdown plus full-precision CSV. Start with fixed configurations, held-out sample-level performance, common-complete event transport, native event summaries and saccade velocity.
- `output/review/figures/`: preprocessing/threshold curves, common SG5/23 comparison, held-out policy/fixed-CNN-branch performance and primary event distributions. Captions retain support restrictions.
- `data/frozen/`: full-precision aggregate results, all CNN branches, alternate policies, historical/gap-aware references, duration sensitivities, transport/scaling details and velocity missingness.
- `output/supplementary/`: additional CNN distribution panels and the detailed Part 1 plotting ledger.
- `validation/`: current independent checks, archived validation evidence and release validation documentation. Archived records are labelled historical and privacy-redacted.

Part 1 classical curves are exploratory cohort grids, **not independently validated optima**. Part 2 classical scores evaluate a training-only, availability-first joint preprocessing policy using 20 leave-one-participant-out folds. Fixation and saccade thresholds may differ within shared preprocessing. CNN results come from separately trained fixed branches with outer participant-held-out predictions; Raw is the provisional primary branch, not a winner selected on held-out MCC.

Part 2B configurations use all development participants; their development scores are in-sample. They are not the same estimand as Part 2 LOPO policy performance. Part 3 uses those fixed native event exports, with explicit trial supports. Unmatched distributional transport is not matched-event accuracy.

## Limits and provenance

Support differs across algorithms, grid configurations and analyses. Original parameters were developed on this cohort. A change in temporal representation under sample-based processing does not isolate a hardware-frequency effect. CNN Original/30-Hz Option A has unequal numbers of unique training windows and updates per epoch. Native CNN duration and Original input exposure differ from the classical pipeline. Historical reference NA/cumsum behaviour is preserved and contrasted with a gap-aware definition. See [METHODS.md](METHODS.md).

The package is staged against GitHub `main` commit `2d2b7d5827c481f3b82296cc5be1f1d2e5a89ef9`. `config/frozen_manifest.csv` maps released data/helpers to their unchanged scientific source hashes. The new presentation layer never reselects parameters, computes bootstrap draws, re-solves transport, trains or runs detectors.

**Release status: CONDITIONALLY READY for author review; not approved for public publication.** Aggregate redistribution and source-code licensing require author confirmation; complete external-input recomputation remains untested. See [PUBLIC_RELEASE_AUDIT.md](PUBLIC_RELEASE_AUDIT.md), [validation/RELEASE_VALIDATION.md](validation/RELEASE_VALIDATION.md) and [PROPOSED_GIT_CHANGES.md](PROPOSED_GIT_CHANGES.md). No Git commit, push, pull request or manuscript change was made.
