# Supplementary results and validation guide

The full-precision scientific supplement is under `data/frozen/`; files are not duplicated here. Paths below are relative to the package root.

| Scientific detail | Location |
|---|---|
| Exact classical grids, full score curves and supports | `data/frozen/part1/` |
| All training-only policy scores, paired supports, aggregate selection frequencies and anonymised fold-selection diagnostics | `data/frozen/part2/` |
| Fixed configuration definitions, stability, development-score interpretation, all CNN branches and descriptive post-hoc branch ranking | `data/frozen/part2b/` |
| Historical/gap-aware transport, common/method-specific/branch supports and scaling | `data/frozen/part3/wasserstein_*.csv`, `reference_scaling.csv` |
| Native/span duration sensitivities and representation contrasts | `data/frozen/part3/duration_convention_sensitivity.csv`, `primary_transport_with_sensitivity_endpoints.csv`, `representation_distribution_contrasts.csv` |
| Event summaries and input exposure | `data/frozen/part3/event_distribution_summary.csv`, `native_input_exposure_summary.csv` |
| Velocity definitions and missingness | `data/frozen/part3/saccade_velocity_summary.csv`, `saccade_velocity_missingness.csv` |
| All archived plot curves and panel/sample denominators | `data/frozen/part3/figure_source_curves.csv`, `config/part3_curve_captions.json` |
| Additional five-CNN-branch event panels | `output/supplementary/CNN_sensitivity_*` PNG/PDF pairs |
| Part 1 plotting ledger and existing selected-policy context | `output/supplementary/part1_presentation/` |
| Archived independent numerical gates (redacted) | `validation/archived/` |
| New independent release checks | `validation/independent_release.json`, `command_tests.json` |

The archived post-hoc branch ranking and full-development fixed scores must not be mistaken for unbiased branch selection or LOPO performance of a single final configuration. Support hashes remain in source tables even where they are too long for compact review tables. Raw observation-level ledgers and private original validation fixtures are deliberately not public; see REPRODUCIBILITY.md for the exact limitation.
