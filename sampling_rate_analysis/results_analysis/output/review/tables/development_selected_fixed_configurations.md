# Provisional review - development selected fixed configurations

Full-precision values: matching CSV. Definitions and support limitations: METHODS.md.

| algorithm | representation | preprocessing | fix_threshold | sacc_threshold | configuration_status | development_evaluation_rows |
| --- | --- | --- | --- | --- | --- | --- |
| IVT | original | SG5_23 | 35 | 20.0 | development_selected | 68577 |
| IDT | original | SG5_23 | 1 | 6.5 | development_selected | 68044 |
| EK03 | original | Mean3 | 3 | 2.0 | development_selected | 68577 |
| IHMM | original | Mean3 | NA | NA | development_selected | 68577 |
| IVT | rate30 | SG5_23 | 25 | 20.0 | development_selected | 68577 |
| IDT | rate30 | Mean3 | 1 | 1.5 | development_selected | 65245 |
| EK03 | rate30 | Raw | 3 | 2.0 | development_selected | 68577 |
| IHMM | rate30 | Median3 | NA | NA | development_selected | 68443 |
| CNN-BLSTM | original | Raw | NA | NA | provisional_requires_scientific_confirmation | NA |
| CNN-BLSTM | rate30 | Raw | NA | NA | provisional_requires_scientific_confirmation | NA |
