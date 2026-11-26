# Dependencies

Archived review/verification: R 4.5.2 with jsonlite 2.0.0 and digest 0.6.39; base graphics/Cairo PNG/vector PDF. Python's standard library suffices only for python/verify_release.py.

Scientific recomputation: Python 3.11.9, NumPy 1.26.4, pandas 2.2.3, SciPy 1.14.1; R packages dplyr 1.2.1, purrr 1.2.2, tidyr 1.3.2, tibble 3.3.1, signal 1.8-1, zoo 1.9-1, eyemovements 0.0.1 and **T4transport 0.1.8**. These are measured local versions, not claims of compatibility with every version. The detector package source identity is the pinned eyemovements commit 87d8e4465418782eb24dd910d3ca24ebb8c10a25, recorded with source hashes. Use the team's archived/pinned package sources for exact restoration; do not silently upgrade numerical implementations.

Install third-party packages into a normal configured R user library and a Python environment. For ordinary available R packages use install.packages with their names; exact historical releases may need the corresponding archived source. Configure R_LIBS_USER/R_LIBS and --python explicitly when using nondefault environments. No library path is hidden in executable package code, and no software is silently installed or vendored. Keep dependencies under their own licences. A clean installation has not been tested here.

The original EK03 export requires attached purrr (unqualified map2); the scientific entry point mirrors the historical runner's attached dependencies. Some locally installed tidyverse packages report a build-under-R-4.5.3 warning on R 4.5.2; actual numerical checks determine whether the scientific results match. Sandbox locale warnings are recorded with the actual C locale. Neither warning is represented as an unexplained scientific failure.

Python 3.9+ path/string APIs are used; the tested environment is 3.11.9. No TensorFlow/Keras/neural framework is imported by this Results pipeline. CNN checkpoints and training resources are unnecessary when using approved saved participant-held-out predictions/events. Stochastic HMM inference is not rerun.

Actual R session and independent Python environment records are saved in each private scientific run. This package has not been certified on a clean machine or a non-Windows platform. Portability of paths and real-data execution are tested separately from installation/platform coverage.
