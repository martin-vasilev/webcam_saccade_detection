#!/usr/bin/env python3
"""
Run Event_Level.py on all four CNN-BLSTM output directories
(raw, mean, median, sg) and save results with distinct filenames.
"""
import os
import sys
import glob
import math
import pandas as pd
import numpy as np

# ---- Config ----
OUTPUT_DIRS = {
    "raw":      "data/outputs_webcam",
    "mean":     "data/outputs_webcam_mean",
    "median":   "data/outputs_webcam_median",
    "sg":       "data/outputs_webcam_sg",
    "sg_p5_n23": "data/outputs_webcam_sg_p5_n23",
}
RESULTS_DIR = "results"

TRUE_COL = "handlabeller_final"
FIX_LABEL = 1
SACC_LABEL = 2

PRED_MAP = {
    "FIX": 1, "FIXATION": 1,
    "SACCADE": 2, "SACC": 2,
    "SP": 3, "NOISE": 4,
    "UNKNOWN": 0, "?": 0,
}

ORIGINAL_COLS = {
    "time", "x", "y", "confidence",
    "handlabeller1", "handlabeller2", "handlabeller_final",
    "speed_1", "direction_1", "acceleration_1",
    "speed_2", "direction_2", "acceleration_2",
    "speed_4", "direction_4", "acceleration_4",
    "speed_8", "direction_8", "acceleration_8",
    "speed_16", "direction_16", "acceleration_16",
}

# ---- Helpers (copied from Event_Level.py) ----
def read_arff(path):
    attrs = []; rows = []; in_data = False
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("%"): continue
            if line.lower().startswith("@attribute"):
                attrs.append(line.split()[1].strip("'\""))
            elif line.lower() == "@data": in_data = True
            elif in_data: rows.append([x.strip() for x in line.split(",")])
    return pd.DataFrame(rows, columns=attrs)

def find_prediction_column(df):
    for c in df.columns:
        if c not in ORIGINAL_COLS: return c
    return df.columns[-1]

def convert_prediction_labels(pred_series):
    raw = pred_series.astype(str).str.strip().str.upper()
    numeric_pred = pd.to_numeric(raw, errors="coerce")
    if numeric_pred.notna().all(): return numeric_pred.astype(int)
    mapped_pred = raw.map(PRED_MAP)
    if mapped_pred.isna().any():
        print("Unmapped:", sorted(raw.unique()))
        raise ValueError("Cannot map prediction labels")
    return mapped_pred.astype(int)

def get_subject(path):
    parent = os.path.basename(os.path.dirname(path))
    if parent.startswith("S"): return parent
    return os.path.basename(path).split("_")[0]

def get_trial(path):
    return os.path.basename(path).replace(".arff", "")

def estimate_sample_step(time_values):
    time_values = np.asarray(time_values, dtype=float)
    if len(time_values) < 2: return 0.0
    diffs = np.diff(time_values)
    diffs = diffs[diffs > 0]
    if len(diffs) == 0: return 0.0
    return float(np.median(diffs))

def extract_events(time_values, labels, target_label):
    time_values = np.asarray(time_values, dtype=float)
    labels = np.asarray(labels, dtype=int)
    if len(time_values) == 0: return []
    sample_step = estimate_sample_step(time_values)
    events = []; in_event = False; start_idx = None
    for i, lab in enumerate(labels):
        if lab == target_label and not in_event:
            in_event = True; start_idx = i
        elif lab != target_label and in_event:
            duration = (time_values[i-1] - time_values[start_idx] + sample_step) / 1000.0
            events.append(duration)
            in_event = False
    if in_event:
        duration = (time_values[-1] - time_values[start_idx] + sample_step) / 1000.0
        events.append(duration)
    return events

def event_summary(time_values, labels, target_label):
    durations = extract_events(time_values, labels, target_label)
    if len(durations) == 0:
        return {"event_count": 0, "mean_duration_ms": 0.0, "sd_duration_ms": 0.0}
    return {
        "event_count": len(durations),
        "mean_duration_ms": float(np.mean(durations)),
        "sd_duration_ms": float(np.std(durations, ddof=1)) if len(durations) > 1 else 0.0,
    }

def rmse(values):
    values = np.asarray(values, dtype=float)
    if len(values) == 0: return np.nan
    return math.sqrt(np.mean(values ** 2))


def process_directory(output_dir, smoothing_name):
    files = sorted(glob.glob(os.path.join(output_dir, "**", "*.arff"), recursive=True))
    print(f"\n{'='*60}")
    print(f"Processing {smoothing_name}: {output_dir} ({len(files)} files)")

    trial_rows = []
    skipped = []

    for path in files:
        df = read_arff(path)
        if "time" not in df.columns or TRUE_COL not in df.columns:
            skipped.append({"file": path, "reason": "missing columns"}); continue
        pred_col = find_prediction_column(df)

        time_values = pd.to_numeric(df["time"], errors="coerce")
        true_labels = pd.to_numeric(df[TRUE_COL], errors="coerce")
        pred_labels = convert_prediction_labels(df[pred_col])
        valid = time_values.notna() & true_labels.notna()
        time_values = time_values[valid].astype(float).to_numpy()
        true_labels = true_labels[valid].astype(int).to_numpy()
        pred_labels = pred_labels[valid].astype(int).to_numpy()

        subject = get_subject(path)
        trial = get_trial(path)

        gt_fix = event_summary(time_values, true_labels, FIX_LABEL)
        gt_sac = event_summary(time_values, true_labels, SACC_LABEL)
        if gt_fix["event_count"] == 0 and gt_sac["event_count"] == 0:
            skipped.append({"subject": subject, "trial": trial, "file": path, "reason": "no GT"})
            continue

        for event_name, event_label in [("fixation", FIX_LABEL), ("saccade", SACC_LABEL)]:
            gt_s = event_summary(time_values, true_labels, event_label)
            pred_s = event_summary(time_values, pred_labels, event_label)
            trial_rows.append({
                "subject": subject, "trial": trial, "file": path,
                "event_type": event_name,
                "smoothing": smoothing_name,
                "gt_event_count": gt_s["event_count"],
                "pred_event_count": pred_s["event_count"],
                "event_count_error": pred_s["event_count"] - gt_s["event_count"],
                "gt_mean_duration_ms": gt_s["mean_duration_ms"],
                "pred_mean_duration_ms": pred_s["mean_duration_ms"],
                "mean_duration_error_ms": pred_s["mean_duration_ms"] - gt_s["mean_duration_ms"],
                "gt_sd_duration_ms": gt_s["sd_duration_ms"],
                "pred_sd_duration_ms": pred_s["sd_duration_ms"],
                "sd_duration_error_ms": pred_s["sd_duration_ms"] - gt_s["sd_duration_ms"],
            })

    trial_df = pd.DataFrame(trial_rows)
    if trial_df.empty:
        print(f"  WARNING: No valid trials for {smoothing_name}")
        return None

    # Save
    os.makedirs(RESULTS_DIR, exist_ok=True)
    out_path = os.path.join(RESULTS_DIR, f"ml_event_level_by_trial_{smoothing_name}.csv")
    trial_df.to_csv(out_path, index=False)

    # Overall RMSE
    overall_rows = []
    for event_type, sub_df in trial_df.groupby("event_type"):
        overall_rows.append({
            "event_type": event_type, "smoothing": smoothing_name, "n_trials": len(sub_df),
            "event_count_rmse": rmse(sub_df["event_count_error"]),
            "mean_duration_rmse_ms": rmse(sub_df["mean_duration_error_ms"]),
            "sd_duration_rmse_ms": rmse(sub_df["sd_duration_error_ms"]),
        })
    overall_df = pd.DataFrame(overall_rows)
    overall_path = os.path.join(RESULTS_DIR, f"ml_event_rmse_overall_{smoothing_name}.csv")
    overall_df.to_csv(overall_path, index=False)

    print(f"  Valid trials: {len(trial_df)//2}, skipped: {len(skipped)}")
    print(f"  Fixation RMSE: n_events={overall_df[overall_df.event_type=='fixation']['event_count_rmse'].values[0]:.1f}, "
          f"dur={overall_df[overall_df.event_type=='fixation']['mean_duration_rmse_ms'].values[0]:.3f}")
    print(f"  Saccade  RMSE: n_events={overall_df[overall_df.event_type=='saccade']['event_count_rmse'].values[0]:.1f}, "
          f"dur={overall_df[overall_df.event_type=='saccade']['mean_duration_rmse_ms'].values[0]:.3f}")
    return trial_df


def main():
    all_trials = []
    for name, dirpath in OUTPUT_DIRS.items():
        if not os.path.isdir(dirpath):
            print(f"SKIP {name}: directory {dirpath} not found")
            continue
        df = process_directory(dirpath, name)
        if df is not None:
            all_trials.append(df)

    # Combined file
    combined = pd.concat(all_trials, ignore_index=True)
    combined_path = os.path.join(RESULTS_DIR, "ml_event_level_by_trial_all_smoothing.csv")
    combined.to_csv(combined_path, index=False)
    print(f"\nCombined: {combined_path} ({len(combined)} rows)")

    # Summary
    print("\n========== SUMMARY ==========")
    for name in OUTPUT_DIRS:
        sub = combined[combined.smoothing == name]
        if len(sub) == 0: continue
        for et in ["fixation", "saccade"]:
            et_sub = sub[sub.event_type == et]
            print(f"  {name:8s} {et:8s}:  n_trials={len(et_sub)//2}, "
                  f"n_rmse={rmse(et_sub.event_count_error):.1f}, "
                  f"dur_rmse={rmse(et_sub.mean_duration_error_ms):.3f}")


if __name__ == "__main__":
    main()
