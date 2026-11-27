"""No-write safety preflight for participant-level scientific outputs."""
from pathlib import Path
import sys,json
package,input_root,target=map(lambda x:Path(x).resolve(),sys.argv[1:4])
protected=[package.parents[1],input_root/'classical_detection_reproduction',input_root/'classical_evaluation_reproduction',input_root/'webcam_saccade_detection-main']
protected += [input_root/'sampling_rate_analysis'/s for s in ['martin_results_extension_001','analysis_b','full_analysis_a_001','extension_c','downstream_original_vs_30hz_001','statistical_inference_a_001']]
assert target!=input_root and not any(target==p or target.is_relative_to(p) for p in protected),'Refusing public/protected output path before creating any directory'
print(json.dumps(dict(output=str(target))))
