# Proposed Git changes and PR description

Base: official main `2d2b7d5827c481f3b82296cc5be1f1d2e5a89ef9`.

## Proposed diff

Add only `sampling_rate_analysis/results_analysis/`. No existing tracked file needs modification. In particular, do not edit manuscript Rmd/HTML, detector source, public data or the root README/ignore file as part of this candidate. `validation/release_inventory.csv` records the proposed files and their hashes, subject to its documented self-reference/mutable-record exclusions.

Excluded from the proposed diff: staging Git metadata, builder/test/private-audit scripts and logs, external-input paths, relocation copy, scientific production folders, raw/event/participant-level data, predictions, neural models, installed libraries and Part 4. The full scientific source workspace is preserved separately. Inspect the package explicitly; never add the parent staging directory or scientific workspace wholesale.

No commit, push, branch publication or pull request is authorised or performed by this task. Licence and unpublished-result clearance remain pre-publication requirements.

## Suggested PR title

Add portable Results Parts 1–3 review package with frozen-result verification

## Suggested PR body

Adds a modular R entry point for verifying frozen aggregate outputs and regenerating provisional review tables/figures for preprocessing sensitivity, held-out sample-level policies, development-selected fixed settings and event-level validation. The package preserves support/selection distinctions, historical reference behaviour, native-duration differences and CNN input-exposure limitations.

Includes full-precision aggregate supplements, scientific source/configuration provenance, independent Python numerical checks, review PNG/vector PDF exports and explicit dependency/reproducibility documentation. No detector execution, neural training, parameter reselection, bootstrap resampling, exact-transport recomputation or manuscript changes are included.

Validation: all five documented R stage selectors passed from staging; aggregate numerical checks passed independently; review regeneration and numerical checks passed after relocation. Original scientific and tracked manuscript files remained unchanged. These tests establish archived-result reuse and review regeneration, **not full scientific recomputation**. Public redistribution/licensing clearance and a portable approved external-input adapter remain outstanding.
