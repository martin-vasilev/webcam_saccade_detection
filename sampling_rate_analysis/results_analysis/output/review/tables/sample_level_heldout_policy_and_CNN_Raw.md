# Provisional review - sample level heldout policy and CNN Raw

Full-precision values: matching CSV. Definitions and support limitations: METHODS.md.

| algorithm | representation | event | MCC | F1 | precision | recall | evaluation_rows | evaluation_support |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| IVT | original | fixation | 0.211 | 0.741 | 0.861 | 0.650 | 68577 | own_policy_paired_joint |
| IVT | original | saccade | 0.182 | 0.333 | 0.219 | 0.693 | 68577 | own_policy_paired_joint |
| IVT | rate30 | fixation | 0.126 | 0.622 | 0.847 | 0.491 | 68577 | own_policy_paired_joint |
| IVT | rate30 | saccade | 0.127 | 0.300 | 0.205 | 0.560 | 68577 | own_policy_paired_joint |
| IDT | original | fixation | 0.263 | 0.732 | 0.886 | 0.623 | 65245 | own_policy_paired_joint |
| IDT | original | saccade | 0.045 | 0.167 | 0.197 | 0.145 | 65245 | own_policy_paired_joint |
| IDT | rate30 | fixation | 0.165 | 0.550 | 0.880 | 0.400 | 65245 | own_policy_paired_joint |
| IDT | rate30 | saccade | 0.166 | 0.311 | 0.265 | 0.376 | 65245 | own_policy_paired_joint |
| EK03 | original | fixation | 0.302 | 0.792 | 0.882 | 0.720 | 68577 | own_policy_paired_joint |
| EK03 | original | saccade | 0.197 | 0.344 | 0.235 | 0.642 | 68577 | own_policy_paired_joint |
| EK03 | rate30 | fixation | 0.310 | 0.781 | 0.890 | 0.697 | 68577 | own_policy_paired_joint |
| EK03 | rate30 | saccade | 0.222 | 0.362 | 0.256 | 0.619 | 68577 | own_policy_paired_joint |
| IHMM | original | fixation | 0.141 | 0.783 | 0.827 | 0.744 | 68396 | own_policy_paired_joint |
| IHMM | original | saccade | -0.015 | 0.170 | 0.141 | 0.216 | 68396 | own_policy_paired_joint |
| IHMM | rate30 | fixation | 0.064 | 0.739 | 0.809 | 0.680 | 68396 | own_policy_paired_joint |
| IHMM | rate30 | saccade | -0.048 | 0.096 | 0.106 | 0.088 | 68396 | own_policy_paired_joint |
| CNN-BLSTM | original | fixation | 0.624 | 0.921 | 0.923 | 0.918 | 68577 | frozen_CNN_common |
| CNN-BLSTM | original | saccade | 0.613 | 0.666 | 0.711 | 0.627 | 68577 | frozen_CNN_common |
| CNN-BLSTM | rate30 | fixation | 0.595 | 0.920 | 0.905 | 0.935 | 68577 | frozen_CNN_common |
| CNN-BLSTM | rate30 | saccade | 0.549 | 0.613 | 0.649 | 0.580 | 68577 | frozen_CNN_common |
