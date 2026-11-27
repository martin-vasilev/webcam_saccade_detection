# Historical implementation archive — inspect, do not execute as a release workflow

These are the validated scientific implementation modules and independent checks from Results Parts 1–3. They are provided for methodological inspection and future input-adapter work. The executable public entry point is `../run_results.R`.

The original workflows expect the original workspace tree and external registry of participant/trial predictions/events/coordinates. That environment is **not shipped or implicitly available**. Do not source these workflows expecting a clean-checkout analysis. Environment-only literal machine paths were redacted in two historical finalisation scripts; all original/released file hashes and exact redaction flags are in `../config/source_code_manifest.json`. Relative historical workspace assumptions remain visible and documented rather than silently treated as working public paths. No archived selection, detector, neural or transport calculation is executed during packaging.

`../R/scientific/` preserves core scientific R helpers byte-for-byte. Only the original metric identity functions are called by the release checks; the rest are inspectable definitions. Original scientific sources remain unchanged in the protected workspace.
