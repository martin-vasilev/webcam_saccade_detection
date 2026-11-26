# Release validation — 25 November 2026

**Technical archived-result/review scope: PASS. Public release: CONDITIONALLY READY. Full scientific recomputation: NOT ESTABLISHED.**

## Executed tests and their meaning

| Test | Outcome | Establishes |
|---|---|---|
| Four authoritative frozen output manifests | PASS | All 228 output entries matched their recorded hashes before packaging |
| Public frozen aggregate/helper manifest | PASS | 48 released data/configuration/helper hashes, including explicitly transformed metadata |
| Historical implementation manifest | PASS | 45 inspectable archived source files with original/release hashes |
| Independent Python confusion checks | PASS | 1,132 aggregate rows: MCC, F1/precision/recall where present and evaluation-row denominators |
| Independent transport identities | PASS | 2,336 standardised raw-distance/similarity identities; source bytes retained, not transport re-solving |
| Review table source matching | PASS | 1,422 displayed source-field values match their frozen source rows |
| Plot source validation | PASS | 712 Part 1 plotted points and all 84,050 archived event-curve points preserved; 12 event figure sets |
| Event/velocity accounting | PASS | Trial totals/count means/defined denominators and all 30 velocity summary/missingness totals |
| Shared primary transport support | PASS | Same support SHA per event across five methods/both rates; historical 516 fixation/366 saccade trials |
| R `--stage part1`, `part2`, `part2b`, `part3`, `all` | PASS | Commands executed from staged package; archived science verified |
| R all-stage review regeneration | PASS | New provisional summaries/figures regenerated from frozen aggregates; no scientific calculations rerun |
| Relocated all-stage review and independent verification | PASS | Package works at a second path from an unrelated working directory; installed libraries remain external dependencies |
| Unsupported recomputation mode | PASS | Rejected with explicit explanation rather than pretending cached checks are recomputation |
| R/Python historical source syntax | PASS | Parse/compile only, not old workflow execution |
| PNG/PDF visual/structural inspection | PASS | Review and sensitivity plots inspected as contact sheets; PNG 300 dpi and vector PDF export checked |
| Protected scientific workspace | PASS | 1,283 files unchanged, no additions there; 1,388 inherited source pins matched |
| Latest tracked manuscript / Git files | PASS | Manuscript blob identities and tracked-file status unchanged |

Machine-readable evidence: `independent_release.json`, `command_tests.json`, `preservation_and_safety.json`, `visual_review.json`, `frozen_source_gates.json`, and `release_inventory.csv`. Archived prior scientific validation evidence is under `archived/` and explicitly redacted. Runtime checks write `last_run.json`; they do not hard-code PASS before the checks run.

The original frozen Part 3 independent gate records 199,462 native events, 124,837 reconstructed velocity records, 504 transport comparisons and 224 independent assignment problems. These are **prior completed scientific checks**, not new raw-input calculations performed for this release. Public release checks verify the archived source identities and aggregate consequences. Neither unavailable observation-level inputs nor expensive exact transport were reconstructed during packaging.

## Commands actually tested

From the package root, with R dependencies installed:

```bash
Rscript run_results.R --stage part1
Rscript run_results.R --stage part2
Rscript run_results.R --stage part2b
Rscript run_results.R --stage part3
Rscript run_results.R --stage all
Rscript run_results.R --stage all --mode review
python python/verify_release.py
```

The review command was also tested using an absolute script path from another directory after copying the package. The independent verifier was rerun after packaging-only verification changes. Test logs and the relocation copy remain outside the proposed Git addition; public records retain commands/scopes/outcomes without local absolute paths.

## Presentation and output hashes

The review layer uses the original Part 1 plotting implementation and immutable event-density/PMF curves. The new sample-level panel and compact tables only select explicitly documented rows/fields, without selecting parameters or transforming scientific estimates. PNG/PDF formatting is provisional. Output/source/code hashes are recorded in `release_inventory.csv`; the inventory excludes itself and mutable runtime/environment/independent-run records to avoid circular or run-dependent checksums. It is not a signed authenticity guarantee. Regenerating PDF metadata on a different run may change bytes: regenerate a new inventory only after verifying unchanged scientific data and plotting values, not to conceal a source mismatch.

## Remaining limitations

- No full scientific recomputation was run; approved external prediction/event/reference inputs and a tested portable adapter are still required.
- No dependency installation on a clean machine or non-Windows platform was tested. Installed R libraries were reused explicitly; versions are recorded.
- Public raw validation/membership ledgers are intentionally excluded, so archived JSON gates are not a complete raw-input public validation bundle.
- No code/data redistribution licence or unpublished-result clearance is inferred. Obtain author approval before a commit/public release.
- Support, cohort-derived parameters, NA/cumsum semantics, native duration/exposure and unequal CNN optimisation-budget caveats remain in METHODS and captions; package validation does not resolve these scientific limitations.
