# Scientific pipeline validation — 26 November 2026

**Pipeline status: PARTIAL (complete calculation paths implemented; full high-cost execution coverage is not established).** This is now an actual R-orchestrated scientific workflow, not an archive-verification wrapper. All tested real-data calculations matched the frozen scientific results. No unexplained numerical/scientific discrepancy remains in the tested scope.

## Stage-by-stage evidence

| Stage | Scientific inputs / executable producer | Implemented | Actual real-data execution and validation | Remaining scope |
|---|---|---|---|---|
| Preprocessing and classical detection | Original/archived 30-Hz gaze; pinned get_xy/detector expressions via R/detector_replay.R | Yes; pilot/full/archived controls | 1,280 calls: two real trials at both rates × 320 configurations; intervals/statuses match archive. Independent 16-group/1,904-coordinate smoothing/time checks passed. | All 792-trial fresh detector replay not run; full replay path is not production/resume-certified. |
| Part 1 grid sample scoring | Native event/status ledgers and common manual reference; python/recompute_counts.py | Yes | All 506,880 classical + 12,672 I-HMM configuration-trial calls rescored; 640 classical and 16 HMM pooled grid rows matched. Inclusive union and independent sweep agree. | HMM stochastic fitting reused from saved seed-0 outputs, not rerun. |
| CNN prediction scoring | Five held-out prediction branches on Original timestamps | Yes | 400 participant/event/rate/branch count rows matched; 1,371,540 binary prediction/availability assertions passed. No neural execution. | Training/inference deliberately outside Results workflow. |
| Part 2 | New trial counts; original R context/selection/evaluation modules | Yes | All 160 LOPO choices, 86,880 candidate pairs and training supports independently verified; 22,700 selected native-interval calls independently reconstructed; 144 pooled policy rows and 20 CNN branch rows matched. | Historical event-specific comparison-policy identities remain explicit frozen inputs; no new CNN selector. |
| Part 2B | Candidate context and newly computed LOPO results; original numeric block | Yes | All eight development-selected classical settings, 8 stability rows and all 180 interpretation rows matched; final table has 10 rows including two provisional CNN Raw entries. | Development scores remain in-sample, not held-out final-setting estimates. |
| Part 3 native/reference | Raw manual samples, fixed native events, actual coordinates/blockers; native_inputs.py and events.R | Yes | 199,462 events, 34,848 explicit call/reference statuses, 69,696 trial-summary rows, both references and native/span definitions reconstructed. Eight native/reference/coordinate/summary files are byte-identical to frozen originals. | Historical NA/cumsum defect and wider Original CNN exposure remain scientifically material. |
| Part 3 velocity | Newly reconstructed events/coordinates, fixed settings; velocities.R | Yes | All 124,837 velocity records, both velocity rules, aggregate speeds and missingness reproduced. Three output CSVs byte-identical; independent Python native/reference/velocity gate passed over 847 registered source identities. | No missing-coordinate interpolation or event-boundary redesign. |
| Part 3 exact transport | Newly computed trial N/Mean/SD; pinned T4transport 0.1.8 | Yes; representative/full controls | Four prespecified requests solved on complete frozen supports; five output rows including the original method-specific alias. Independent SciPy exact assignment checked three distinct multivariate problems. Distances, similarities, scales and support IDs matched. | The full 504-comparison table, gap/span transport sensitivities and full scientific density-curve regeneration were not rerun in this connected execution. Full-scope path is implemented but not certified by this test. |
| Scientific review regeneration | New scientific tables, R/scientific_review.R and original plotting helpers | Yes | Parts 1/2/2B regenerated from new products: seven PNG/vector-PDF pairs, nine CSV source/compact tables and four Markdown tables; 696 Part 1 plotted values/supports independently checked; visual and DPI/PDF checks passed. | Part 3 scientific-review command correctly refuses representative-only transport instead of borrowing cached curves. Complete existing public Part 3 review remains preserved. |

## Exact transport real-data cases

| Comparison | Trials | Exact calculation | Frozen match |
|---|---:|---|---|
| fixation / primary_five_common_complete / IVT Original | 516 | Multivariate and univariate W1 | PASS |
| fixation / count_only_common_available / IVT Original | 789 | Univariate count W1 | PASS |
| saccade / primary_five_common_complete / IVT Original | 366 | Multivariate and univariate W1 | PASS |

These are full-support comparisons, not small trial fixtures. The independent equal-mass Euclidean assignment solver uses the same mathematical W1 objective but is separate from the R solver. Historical method-specific fixation support has 747 trials, primary common fixation 516 and primary common saccade 366; the count-only support is reported above. No approximate transport was used. The duplicate method-specific alias is not a fourth independent multivariate solve. Source logs print traversed reference/duration blocks even when their distance requests were filtered; **actual execution coverage is the explicit four requests/five rows, not every printed block**.

## Frozen equality and provenance

Machine-readable evidence is in scientific_recomputation/. frozen_comparison.json lists every tested scientific product/row count. native_byte_identities.json and velocity_byte_identities.json demonstrate literal full-file equality after actual recalculation, not file copying. Independent source/selection/transport verifiers operate on the newly produced data. scientific_function_identities.json confirms identical bodies/formals for 14 original scientific functions. executable_code_manifest.json records 19 unchanged/extracted/adapted source files; changes to transport are dependency lookup and explicit execution-subset filtering, not support/metric definitions. New adapters are labelled as such.

No final aggregate table is substituted for a missing recomputation input. Original per-trial ledgers are legitimate standalone selection intermediates; an all-stage run replaces them with event/prediction-derived counts. Frozen result tables are read for independent comparison and the separate archived review mode only. Inherited provenance-only artifacts may be hash-checked without contributing scores.

## Commands actually exercised

From the staged entry point, with explicit authorised paths and installed dependencies:

```bash
Rscript run_results.R --stage all --mode recompute --input-root "INPUT_BUNDLE" --output-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE" --detector-scope pilot --transport-scope representative
Rscript run_results.R --stage all --mode verify --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part1 --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part2 --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
Rscript run_results.R --stage part2b --mode review --scientific-root "PRIVATE_RUN" --python "PYTHON_EXECUTABLE"
```

The all-stage real computation used the staged code with an explicit external input root, not the original development entry point. Verification and scientific review commands were executed with the staging package as working directory. A stage-part1 recompute-mode continuation was also tested there: it explicitly reused its own verified actual count checkpoint and reran the real-data independent smoothing/identity checks; it was **not** counted as a second fresh full scoring pass. Stage-specific scientific branches were exercised within the all-stage run; separate fresh invocations of Part 2/2B/3 were not redundantly rerun solely to test dispatch.

Missing arguments/inputs, public/protected output destinations, source syntax, a relocated cached verifier and partial-Part-3-review refusal were tested separately. Those are interface/structural tests, not scientific computations. See scientific_interface_tests.json. A clean dependency installation and a non-Windows platform were not tested.

## Recovery, costs and safeguards

Two packaging/environment problems were diagnosed and resolved: the original EK03 export requires attached purrr/map2, and the isolated portable Python requires an explicit script-directory import for the bundled metric module. Completed 1,280-call replay outputs were sealed and reused after the Python import repair; they were not unnecessarily rerun. Frozen scientific choices never changed. The private diagnostic records remain available. A Pandas mixed-type import warning and R build/locale warnings were retained; relevant discrete counts, temporal/reference identities and numerical outputs passed the independent checks.

Full count reconstruction took approximately 16 minutes in the tested local environment; native/reference/velocity reconstruction and independent checks were also substantial real-data work. Full detector replay and all exact transport costs were not benchmarked as a connected run; no unsupported runtime extrapolation is claimed. Scientific files live in a private output root outside the Git checkout. Only aggregate validation evidence is copied here. Hash-verified replay/count/native-velocity checkpoints support continuation; later dispatchers are not claimed to be a distributed production scheduler.

## Preservation and remaining actions

The original scientific extension's 1,283 files and 1,388 inherited source hashes remained unchanged. Existing manuscript Rmd/HTML still match the staged main blobs. All 87 pre-existing release output files remained unchanged; their scientific tables/figures are preserved, with new private scientific review products separate. No participant-level data, weights or training resources entered the proposed addition. No commit/push/PR/manuscript edit occurred.

Remaining work is explicit: a clean installation/platform test, production-scale fresh detector replay if desired, and full connected exact-transport/curve/sensitivity execution before claiming every full path is newly rerun. All necessary registered inputs were found locally (847 unique artifact IDs; 874 stage-registry rows); public availability/redistribution is **not** established. Author code/aggregate licensing approval is still needed before publication. No new methodological decision, threshold optimisation or CNN training is needed to interpret the tests reported here.
