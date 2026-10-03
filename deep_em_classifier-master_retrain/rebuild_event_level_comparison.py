"""Rebuild the traditional-algorithm event-level comparison using the correct full GT
(webdata_manual_labels.csv, run-length = 5,810/4,935).
Algorithm events still come from the already-generated detected_events; GT events are
extracted from the full sample-level labels (without blink filtering).
"""
import pandas as pd, numpy as np

ROOT = 'webcam_saccade_detection/manuscript/results'
GT = 'webcam_saccade_detection/data/manual_labels/webdata_manual_labels.csv'

gt = pd.read_csv(GT).sort_values(['sub', 'Trial_Id', 'time_start'])

# ---- Extract GT events from the full sample-level labels (run-length, one event per run) ----
def gt_events(label):
    sub_l = gt['sub'].tolist(); tr_l = gt['Trial_Id'].tolist()
    lab = (gt['ground_truth'] == label).astype(int).tolist()
    t = gt['time_start'].tolist()
    rows = []
    for i in range(len(gt)):
        is_start = (lab[i] == 1) and (i == 0 or sub_l[i] != sub_l[i-1] or tr_l[i] != tr_l[i-1] or lab[i-1] == 0)
        if is_start:
            j = i
            while j + 1 < len(gt) and sub_l[j+1] == sub_l[i] and tr_l[j+1] == tr_l[i] and lab[j+1] == 1:
                j += 1
            rows.append((sub_l[i], tr_l[i], t[i], t[j], t[j] - t[i]))
    return pd.DataFrame(rows, columns=['sub', 'Trial_Id', 'start', 'end', 'duration'])

for lab in ['fixation', 'saccade']:
    ev = gt_events(lab)
    print(f'GT {lab} events: {len(ev)}')

# ---- GT summary per (sub, Trial_Id) ----
def gt_summary(label):
    ev = gt_events(label)
    return ev.groupby(['sub', 'Trial_Id']).agg(
        n=('duration', 'size'),
        mean_dur=('duration', 'mean'),
        sd_dur=('duration', 'std')
    ).reset_index()

gt_fix = gt_summary('fixation')
gt_sac = gt_summary('saccade')

# ---- Algorithm event summary (already generated) ----
trial_list = gt[['sub', 'Trial_Id']].drop_duplicates()

def rebuild(algo_events_csv, gt_sum, out_csv, n_col, m_col, s_col,
            gn_col, gm_col, gs_col, en_col, em_col, es_col):
    algo = pd.read_csv(algo_events_csv)
    raw = algo.groupby(['method', 'smoothing', 'parameter', 'sub', 'Trial_Id']).agg(
        n=('duration', 'size'),
        mean_dur=('duration', 'mean'),
        sd_dur=('duration', 'std')
    ).reset_index()
    params = algo[['method', 'smoothing', 'parameter']].drop_duplicates()
    # Fill missing (parameter x trial) combinations with zeros
    full = params.assign(key=1).merge(trial_list.assign(key=1), on='key').drop(columns='key')
    summ = full.merge(raw, on=['method', 'smoothing', 'parameter', 'sub', 'Trial_Id'], how='left')
    summ[['n', 'mean_dur', 'sd_dur']] = summ[['n', 'mean_dur', 'sd_dur']].fillna(0)
    comp = summ.merge(
        gt_sum.rename(columns={'n': gn_col, 'mean_dur': gm_col, 'sd_dur': gs_col}),
        on=['sub', 'Trial_Id'], how='left')
    comp = comp.rename(columns={'n': n_col, 'mean_dur': m_col, 'sd_dur': s_col})
    comp[en_col] = comp[n_col] - comp[gn_col]
    comp[em_col] = comp[m_col] - comp[gm_col]
    comp[es_col] = comp[s_col] - comp[gs_col]
    # Keep the same column order as the original
    comp = comp[['method', 'smoothing', 'parameter', 'sub', 'Trial_Id',
                 n_col, m_col, s_col, gn_col, gm_col, gs_col, en_col, em_col, es_col]]
    comp.to_csv(out_csv, index=False)
    print(f'  {out_csv}: rows={len(comp)}, GT {gn_col} sum={comp[gn_col].sum()}')

rebuild(
    f'{ROOT}/detected_events/all_fixation_events.csv', gt_fix,
    f'{ROOT}/event_level_rmse/fixation_event_level_comparison.csv',
    'algo_n_fixations', 'algo_mean_fix_dur', 'algo_sd_fix_dur',
    'GT_n_fixations', 'GT_mean_fix_dur', 'GT_sd_fix_dur',
    'error_n_fixations', 'error_mean_fix_dur', 'error_sd_fix_dur')

rebuild(
    f'{ROOT}/detected_events/all_saccade_events.csv', gt_sac,
    f'{ROOT}/event_level_rmse/saccade_event_level_comparison.csv',
    'algo_n_saccades', 'algo_mean_sacc_dur', 'algo_sd_sacc_dur',
    'GT_n_saccades', 'GT_mean_sacc_dur', 'GT_sd_sacc_dur',
    'error_n_saccades', 'error_mean_sacc_dur', 'error_sd_sacc_dur')

print('\nDone. GT now from full webdata_manual_labels (fix=5810, sac=4935).')
