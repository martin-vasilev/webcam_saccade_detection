"""Compute sample-level MCC for every smoothing from the LOO main-model predictions
(blstm_model.py -> outputs_webcam*).
Writes ml_mcc_loo.csv, used for the CNN-BLSTM row in Figure 5A, aligned with
Figure 5B / event-level / main-text conventions.
"""
import pandas as pd, glob, os, re, numpy as np
from sklearn.metrics import matthews_corrcoef

GT = 'webcam_saccade_detection/data/manual_labels/webdata_manual_labels.csv'
OUT = 'ml_mcc_loo.csv'

def gt_num(l):
    return {'fixation': 1, 'saccade': 2, 'pso': 3, 'blink': 4}.get(l, 0)

PMAP = {'FIX': 1, 'SACCADE': 2, 'SP': 3, 'NOISE': 4, 'UNKNOWN': 0,
        'BLINK': 4, 'PSO': 3}

DIRS = [
    ('raw',       'data/outputs_webcam'),
    ('mean',      'data/outputs_webcam_mean'),
    ('median',    'data/outputs_webcam_median'),
    ('sg_p3_n7',  'data/outputs_webcam_sg'),
    ('sg_p5_n23', 'data/outputs_webcam_sg_p5_n23'),
]

gt = pd.read_csv(GT)
gt['gt_num'] = gt['ground_truth'].map(gt_num)

rows = []
for name, d in DIRS:
    all_gt, all_pred = [], []
    for f in glob.glob(f'{d}/S*/*.arff'):
        fn = os.path.basename(f)
        m = re.match(r'S(\d+)_E\d+I(\d+)D', fn)
        if not m:
            continue
        sub, trial = int(m.group(1)), int(m.group(2))
        with open(f) as fh:
            in_data = False
            pred = []
            for line in fh:
                line = line.strip()
                if line.lower() == '@data':
                    in_data = True
                    continue
                if not in_data or not line or line.startswith('%'):
                    continue
                pred.append(line.split(',')[-1].strip())
        g = gt[(gt['sub'] == sub) & (gt['Trial_Id'] == trial)]
        if len(g) == 0:
            continue
        gt_seq = g['gt_num'].values
        pred_seq = [PMAP.get(p, 0) for p in pred[:len(gt_seq)]]
        if len(pred_seq) < len(gt_seq):
            pred_seq += [0] * (len(gt_seq) - len(pred_seq))
        gt_seq = gt_seq[:len(pred_seq)]
        all_gt += list(gt_seq)
        all_pred += list(pred_seq)

    ag = np.array(all_gt)
    ap = np.array(all_pred)
    fm = matthews_corrcoef((ag == 1).astype(int), (ap == 1).astype(int))
    sm = matthews_corrcoef((ag == 2).astype(int), (ap == 2).astype(int))
    print(f'{name:10s} fix={fm:.4f} sac={sm:.4f} (n={len(ag)})')
    rows.append({'method': 'CNN-BLSTM', 'event': 'fixation',
                 'smoothing': name, 'parameter_type': 'none',
                 'parameter_value': None, 'MCC': fm})
    rows.append({'method': 'CNN-BLSTM', 'event': 'saccade',
                 'smoothing': name, 'parameter_type': 'none',
                 'parameter_value': None, 'MCC': sm})

df = pd.DataFrame(rows)
df.to_csv(OUT, index=False)
print(f'\nSaved -> {OUT}')
