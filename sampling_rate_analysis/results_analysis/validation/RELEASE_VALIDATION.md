# Current release validation

The packaging-only validation of 25 November is retained unchanged under history/release_packaging_20261125/. Its claims apply to that earlier archive/review package.

Current scientific execution and frozen numerical comparisons: [SCIENTIFIC_PIPELINE_VALIDATION.md](SCIENTIFIC_PIPELINE_VALIDATION.md). Current code/source hashes: ../config/executable_code_manifest.json and ../config/source_code_manifest.json. Current interface tests: scientific_interface_tests.json; scientific function identity checks: scientific_function_identities.json. Preservation/safety: preservation_and_safety.json and existing_review_preservation.json. Existing archived aggregate/figure validation remains in independent_release.json and archived/.

The current inventory is release_inventory.csv. It excludes itself and mutable runtime/environment/independent-run records (last_run.json, independent_release.json, environment/sessionInfo.txt, environment/versions.csv) to avoid circular/run-dependent checksums. Old historical inventories are evidence of old snapshots, not current file checksums. PDF bytes may change across rendering sessions/platforms; that is separate from scientific numeric equivalence.

Technical tested-scope checks PASS. Pipeline execution coverage PARTIAL: full fresh detector replay and all transport/curve branches have not been rerun in this connected execution. The complete publication scope is now author-confirmed (26 November 2026); final functional and rendering validation is recorded in FINAL_RELEASE_VALIDATION.md. Public push is not authorised by the local commit. These statuses must not be conflated.

Final verifier packaging clarification: PDF/PNG snapshot-byte differences are reported separately from scientific validation. Default verification keeps every non-render inventory hash strict, verifies all frozen/code manifests and scientific numerical/plot-source checks, and reports `render_hash_differences` plus `render_review_required`. `--strict-render-bytes` also requires unchanged render bytes. This changes packaging validation only, not any scientific metric, detector, model selection, event definition or frozen source.
