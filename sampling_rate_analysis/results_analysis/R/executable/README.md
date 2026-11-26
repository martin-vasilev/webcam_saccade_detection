# Executable scientific helpers

The single supported orchestrator is `../../run_results.R` through `../scientific_pipeline.R`. Functions here implement the original candidate context, selection, evaluation, event summaries, velocities, transport and distribution calculations. The lineage manifest is `../../config/executable_code_manifest.json`.

Some original module files retain their old loader/finalisation function definitions for source fidelity (for example `part2_inputs` and `run_part2b`). Those legacy dispatchers are **not invoked** by the new entry point and are not standalone public commands. The new resolver supplies explicit external inputs/private outputs, and `part2b/calculate.R` executes the original numeric block without old commit/reuse dispatch. Do not substitute the legacy loaders for the validated interface.

Four pure historical extraction/helper files are bundled byte-for-byte under `historical/`. Detector implementations remain the pinned installed eyemovements dependency, not a simplified rewrite. Scientific code-body identities and numerical validation are recorded separately; inspection/parse tests alone are not scientific recomputation evidence.
