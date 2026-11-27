# Public-release safety and licensing audit

**Status: CONDITIONALLY READY for research-team review, not cleared for public publication.**

## Contents and exclusion decisions

The proposed addition contains aggregate CSVs/curves, scientific definitions and inspectable source code, provisional review plots/tables, manifests and validation documentation. No neural checkpoints, weights, caches, training environments, raw coordinate samples, sample predictions, event-level exports or identifiable participant/trial observation records have been copied. Fold selection diagnostics omit held-out participant IDs. Per-fold training support hashes remain provenance evidence; individual participant performance/membership ledgers are excluded. Code still describes participant-held-out logic, which is methodological documentation rather than participant observations.

The source scientific aggregate tables were authorised for local packaging and review by the user. This is **not evidence of permission to redistribute all unpublished aggregate results or code publicly**. Authors must confirm permission before a commit/public release. Anonymous aggregate curves can still be unpublished research material. No public upload has occurred.

The audited GitHub main tree has no root LICENSE file. Do not assign a permissive licence to other authors' contributions or assume GitHub visibility grants redistribution rights. Obtain the team's chosen licence and any required attribution/third-party clearance. Installed R/Python packages, detector package sources and neural frameworks are not vendored. Existing third-party implementations are referenced by pinned provenance, not repackaged as newly authored code. Historical helper/source code in this candidate also requires author review and attribution approval.

## Portability and static checks

Executable release code uses its script directory and project-relative files, with dependencies supplied through the R library mechanism. No personal machine paths, credentials matching the documented token/private-key patterns, participant/trial observation columns or files above 20 MiB were found in the proposed addition. A source metadata column containing a package-source absolute path was changed to an `external_package_source/` alias; its scientific values are unchanged and both source and released hashes are recorded. Historical source archives contain two environment-only literal path redactions; their exact source/release hashes are in `config/source_code_manifest.json`. They are not executable release workflows.

Static scans are bounded checks, not proof that arbitrary text cannot contain sensitive information. The team must review permission and unpublished-material status. Machine-specific logs, the build scripts, full private input paths, the independent relocation test copy, contact sheets and the original protected hash baseline remain **outside** the proposed repository addition.

## Git and preservation

Staging is a sparse clone of official main at `2d2b7d5827c481f3b82296cc5be1f1d2e5a89ef9`. Only README, ignore rules and the latest manuscript Rmd/HTML were materialised from the existing remote tree. Sparse/lazy checkout avoids downloading model/data artifacts. The proposed package is untracked; no existing tracked file was changed. Original scientific files are checked against a pre-packaging baseline and inherited frozen source pins. See `validation/preservation_and_safety.json` for measured counts.

No commit, remote branch, push, PR, original-file reset or manuscript edit was performed. The clone's local checkout is confined to the newly created staging directory. It did not overwrite the extracted working project.

## Remaining clearance items

1. Confirm public redistribution of the aggregate result tables, curves and scientific helper/source archive; choose a licence and attribution policy.
2. Decide which derived participant-level inputs may be deposited, under which access/consent conditions. Do not make raw-input data public solely to make a command runnable.
3. Supply/approve external artifact locations if full event/prediction recomputation is required. Wire and validate that adapter as a separately authorised step; current commands honestly support archived-result verification and review regeneration only.
4. Authorise the exact staged diff before any commit or publication. Existing manuscript files should remain unchanged.
