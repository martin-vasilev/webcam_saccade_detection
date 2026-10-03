"""Compute CNN-BLSTM statistics from the LOO model predictions (20 subjects, median smoothing).
Computes: overall MCC, per-participant MCC range, label distribution.
Uses the same outputs_webcam_median predictions as the Figure 5B / event-level analyses.
"""
import pandas as pd, glob, os, re, numpy as np
from sklearn.metrics import matthews_corrcoef

GT = 'webcam_saccade_detection/data/manual_labels/webdata_manual_labels.csv'
PRED_DIR = 'data/outputs_webcam_median'

def gt_num(l):
    if l == 'fixation': return 1
    if l == 'saccade': return 2
    if l == 'pso': return 3
    if l == 'blink': return 4
    return 0  # unclear/NA

PMAP = {'FIX': 1, 'SACCADE': 2, 'SP': 3, 'NOISE': 4, 'UNKNOWN': 0,
        'BLINK': 4, 'PSO': 3}

gt = pd.read_csv(GT)
gt['gt_num'] = gt['ground_truth'].map(gt_num)

all_gt, all_pred = [], []
per_sub = {}
pred_dist = {}

for f in sorted(glob.glob(f'{PRED_DIR}/S*/*.arff')):
    fn = os.path.basename(f)
    m = re.match(r'S(\d+)_E\d+I(\d+)D', fn)
    if not m:
        continue
    sub, trial = int(m.group(1)), int(m.group(2))
    rows = []
    with open(f) as fh:
        in_data = False
        for line in fh:
            line = line.strip()
            if line.lower() == '@data':
                in_data = True
                continue
            if not in_data or not line or line.startswith('%'):
                continue
            rows.append(line.split(','))
    g = gt[(gt['sub'] == sub) & (gt['Trial_Id'] == trial)]
    if len(g) == 0:
        continue
    gt_seq = g['gt_num'].values
    pred_seq = [PMAP.get(r[-1].strip(), 0) for r in rows[:len(gt_seq)]]
    if len(pred_seq) < len(gt_seq):
        pred_seq += [0] * (len(gt_seq) - len(pred_seq))
    gt_seq = gt_seq[:len(pred_seq)]
    all_gt += list(gt_seq)
    all_pred += list(pred_seq)
    per_sub.setdefault(sub, {'gt': [], 'pred': []})
    per_sub[sub]['gt'] += list(gt_seq)
    per_sub[sub]['pred'] += list(pred_seq)
    for lbl in [r[-1].strip() for r in rows[:len(gt_seq)]]:
        pred_dist[lbl] = pred_dist.get(lbl, 0) + 1

all_gt = np.array(all_gt)
all_pred = np.array(all_pred)

print('=== Overall sample counts ===')
print(f'GT fixation: {int((all_gt==1).sum())}, GT saccade: {int((all_gt==2).sum())}, GT total: {len(all_gt)}')
for lbl in ['FIX', 'SACCADE', 'SP', 'NOISE', 'UNKNOWN']:
    print(f'Predicted {lbl}: {pred_dist.get(lbl, 0)}')

print('\n=== Overall MCC (one-vs-rest, sample-level) ===')
fix_mcc = matthews_corrcoef((all_gt == 1).astype(int), (all_pred == 1).astype(int))
sac_mcc = matthews_corrcoef((all_gt == 2).astype(int), (all_pred == 2).astype(int))
print(f'Fixation MCC: {fix_mcc:.4f}')
print(f'Saccade MCC:  {sac_mcc:.4f}')

print('\n=== Per-participant MCC ===')
f_list, s_list = [], []
for sub in sorted(per_sub):
    g = np.array(per_sub[sub]['gt'])
    p = np.array(per_sub[sub]['pred'])
    fm = matthews_corrcoef((g == 1).astype(int), (p == 1).astype(int))
    sm = matthews_corrcoef((g == 2).astype(int), (p == 2).astype(int))
    f_list.append(fm)
    s_list.append(sm)
    print(f'S{sub}: fixation {fm:.3f}, saccade {sm:.3f}')
print(f'\nFixation MCC range: {min(f_list):.3f} - {max(f_list):.3f}')
print(f'Saccade MCC range:  {min(s_list):.3f} - {max(s_list):.3f}')
