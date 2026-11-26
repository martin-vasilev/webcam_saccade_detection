"""Independent real-data smoothing/anchor check, not a production preprocessing implementation.

Uses the same SciPy/centred-window definitions as the frozen independent Phase 3 verifier.
"""
from pathlib import Path
import sys,json
import numpy as np,pandas as pd
from scipy.signal import savgol_filter
run=Path(sys.argv[1]);paths=json.loads((run/'input_paths.json').read_text())
read=lambda p:pd.read_csv(p,float_precision='round_trip')
actual=read(run/'detector_replay/smoothed_coordinates.csv');truth=read(paths['truth']);truth=truth[(truth.conf>0)&(truth.el_pupil>0)]
checked=0;groups=0
for (condition,s,t,method),d in actual.groupby(['condition','sub','Trial_Id','smoothing'],sort=False):
 g=truth[(truth['sub']==s)&(truth.Trial_Id==t)] if condition=='original' else read(paths[f'coordinates_sub{s}_trial{t}'])
 xy=g[['x','y']].to_numpy().copy();n=len(g)
 if n>=5:
  if method in ['mean','median']:
   smoothed=np.full_like(xy,np.nan)
   for j in range(1,n-1):smoothed[j]=np.mean(xy[j-1:j+2],axis=0) if method=='mean' else np.median(xy[j-1:j+2],axis=0)
   xy=smoothed
  elif method=='sg' and n>=23:xy=savgol_filter(xy,23,5,axis=0,mode='interp')
 np.testing.assert_allclose(d.time,g.time_start,rtol=0,atol=1e-9)
 np.testing.assert_allclose(d[['x','y']],xy,rtol=1e-7,atol=2e-5,equal_nan=True)
 checked+=len(g);groups+=1
result=dict(passed=True,groups=groups,coordinate_rows=checked,source_timestamps_exact=True,absolute_tolerance=2e-5,relative_tolerance=1e-7,production_detector_calls=0)
(run/'validation/independent_preprocessing.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result))
