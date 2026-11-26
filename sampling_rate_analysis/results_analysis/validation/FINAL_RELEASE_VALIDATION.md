# Final authorised release validation — 26 November 2026

**Scientific, provenance and visual checks: PASS for this local release.** The author-confirmed scope includes the complete existing package, LOPO diagnostics, four historical helper extracts, aggregate tables and provisional figures. No reduced code-only package was substituted. Remote push/PR remains separately approval-gated.

## Execution from the actual release candidate

```bash
Rscript run_results.R --stage all --mode verify
Rscript run_results.R --stage all --mode review
python -B python/verify_release.py
```

All passed. The candidate's `--scientific-root` verification also passed against an isolated copy of the existing scientific checkpoints. This verifies checkpoint hashes and frozen numerical comparisons; it does not rerun scientific computations. The original private run is unchanged.

Independent checks cover 48 frozen hashes, 1,132 classification rows, 2,336 distance/similarity identities, 1,422 review cells, 84,050 curve points, 712 Part 1 plot values, 45 archived source hashes and 19 executable source hashes. Configuration IDs, scoring supports and full-precision tables match their frozen originals. All 143 checked scientific source/configuration/non-render output files are byte-identical. Eight compact review CSV tables are byte-identical. The four historical helpers and all frozen selections/results are preserved.

## Rendering and presentation

All 26 PNGs were inspected; the changed figures were compared with frozen versions. Eleven PNGs differ at the raster level; the remaining fifteen are byte-identical. PDF extracted text is exact for all 26; decoded page streams are exact for 25, with one explicitly reviewed margin-only exception. Metadata excluding CreationDate/ModDate is exact. The sample-level MCC panel's left margin was increased from 3.5 to 4.2 lines to avoid clipping its y-axis labels; metrics, supports and plotted observations are unchanged. Provisional density tables/curves, panels and scientific groupings are unchanged. See final_render_review.json for original/current image hashes and observations.

The independent verifier retains strict byte checks for all non-render files, all scientific source/configuration manifests and frozen results. Default mode reports PNG/PDF snapshot differences separately and explicitly requires render review. Negative tests confirmed that it rejects a changed frozen scientific input and that `--strict-render-bytes` rejects changed images. After scientific/visual validation, the release inventory was reissued for the final assembled files. The original inventory and comparison evidence remain in the private release-preparation records. Use the following to check this unchanged snapshot:

```bash
python -B python/verify_release.py --strict-render-bytes
```

After future review regeneration, inspect any reported rendering differences before issuing another snapshot. A numerical PASS alone never approves an unreviewed image. Do not refresh scientific hashes to hide a discrepancy. Runtime environment/last-run records are excluded from the snapshot inventory as documented in RELEASE_VALIDATION.md.

## Workflow and boundaries

The unified R entry point and all executable Parts 1–3 scientific modules remain intact. External authorised inputs use the existing exact source/schema/hash contracts. Outputs from scientific recomputation must stay in an explicit private output root. No training, inference, detector replay, bootstrap, parameter selection or transport was run during final release preparation. R 4.5.2 and Python 3.11.9 were used with the existing pinned dependency environment; locale warnings were logged and did not change numerical identities.

Previously verified representative execution remains PARTIAL in its full-production coverage: 1,280 detector pilot calls, full saved-event/prediction scoring, selection/event/velocity reconstruction and representative exact transport were validated earlier. Fresh all-trial detector replay and all 504 transport comparisons have not been rerun. The full paths are not claimed to be newly certified. Clean installation and non-Windows execution remain untested. Historical reference semantics, support differences and CNN optimisation-budget limitations remain as documented in METHODS.md and SCIENTIFIC_PIPELINE_VALIDATION.md.

Original scientific files, source package and Martin's manuscript are preserved. Final exact commit/file inventory and local commit SHA are reported outside the package to avoid self-referential commit metadata. No remote publication has occurred.
