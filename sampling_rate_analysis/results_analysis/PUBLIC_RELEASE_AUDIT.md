# Public-release safety audit â€” scientific pipeline extension

**Authorised release scope confirmed on 26 November 2026; see the dated update below.** Scientific execution coverage is reported separately in validation/SCIENTIFIC_PIPELINE_VALIDATION.md. The prior packaging-only audit is retained under validation/history/release_packaging_20261125/.

The existing staged checkout and complete frozen review outputs are preserved. New additions are executable scientific R/Python modules, explicit source-relative input schemas/hashes, orchestrators, independent checks and documentation. Historical helper code is bundled with exact provenance; installed third-party libraries are not vendored. No participant data, raw coordinates, predictions, native event records, checkpoints, model weights or training environments belong in the proposed Git diff. Source filenames/schemas and pseudonymous trial/participant field names are methodological metadata, not observation records.

Scientific calculations write to an explicitly configured private output root outside the candidate Git repository and protected scientific directories. A no-write preflight rejects protected/public destinations before directory creation. Private input mappings, event/coordinate products, fold-level calculations and logs are retained outside the proposed addition. Do not add the parent staging directory, private_scientific/, portability copies, builder/test scripts or local provenance paths to Git.

The default archived-review mode remains distinct from scientific recomputation. Recompute mode never fills missing scientific inputs with final aggregate results. Full private outputs can contain participant-level information; only aggregate validation evidence is copied into the candidate. Schema/hash catalogs disclose exact input identities without asserting permission to redistribute the underlying files.

The official base main tree did not contain a root LICENSE file. Author confirmation is still needed for public redistribution of unpublished aggregates, original helper/source code and a suitable licence/attribution policy. Repository visibility and local task authorisation do not resolve those rights. Detector packages and numerical dependencies remain separate installations under their own licences. No public upload has occurred.

Static checks cover machine-specific paths, common credential/private-key patterns, observation-level data columns, unexpected large artifacts, Git tracked-file changes and scientific-source preservation. Such scans are bounded checks, not a legal/privacy guarantee. The team must still approve the exact addition and unpublished-material status. Existing review and original science hashes are checked independently; new scientific computations do not replace the frozen references.

No commit, pull, reset, push, remote branch, PR or manuscript edit is performed. Only sampling_rate_analysis/results_analysis/ is proposed as a Git addition. Release inventory exclusions and mutable-runtime records are documented in validation/RELEASE_VALIDATION.md. Public-data permission, clean installation/cross-platform testing and full-path execution coverage must not be inferred from successful local representative tests.


## Authorised release update — 26 November 2026

Martin has confirmed publication permission, as recorded by the project's submitting author. The release scope explicitly includes the existing LOPO fold-level diagnostics, four historical helpers, aggregate results and provisional review figures. Existing attribution and source hashes are preserved. Earlier conditional permission statements in historical audit records describe their dates and are superseded for this authorised release. No new blanket licence is assigned to third-party dependencies.

Only the local release commit is authorised here. Remote push and Pull Request creation require separate approval. No participant input bundle, model weights or private scientific run directory is included.
