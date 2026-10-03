import os
import glob
import math

import pandas as pd
import numpy as np


# =========================
# Paths
# =========================

OUTPUT_DIR = "data/outputs_webcam"
RESULTS_DIR = "results"

os.makedirs(RESULTS_DIR, exist_ok=True)


# =========================
# Settings
# =========================

TRUE_COL = "handlabeller_final"

FIX_LABEL = 1
SACC_LABEL = 2

PRED_MAP = {
    "FIX": 1,
    "FIXATION": 1,
    "SACCADE": 2,
    "SACC": 2,
    "SP": 3,
    "NOISE": 4,
    "UNKNOWN": 0,
    "?": 0,
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


# =========================
# ARFF reader
# =========================

def read_arff(path):
    attrs = []
    rows = []
    in_data = False

    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            line = line.strip()

            if not line or line.startswith("%"):
                continue

            if line.lower().startswith("@attribute"):
                parts = line.split()
                attrs.append(parts[1].strip("'\""))

            elif line.lower() == "@data":
                in_data = True

            elif in_data:
                rows.append([x.strip() for x in line.split(",")])

    return pd.DataFrame(rows, columns=attrs)


def find_prediction_column(df):
    candidate_cols = [c for c in df.columns if c not in ORIGINAL_COLS]

    if candidate_cols:
        return candidate_cols[-1]

    return df.columns[-1]


def convert_prediction_labels(pred_series):
    raw = pred_series.astype(str).str.strip().str.upper()

    numeric_pred = pd.to_numeric(raw, errors="coerce")
    if numeric_pred.notna().all():
        return numeric_pred.astype(int)

    mapped_pred = raw.map(PRED_MAP)

    if mapped_pred.isna().any():
        print("Unmapped prediction values:")
        print(sorted(raw.unique()))
        raise ValueError("Some prediction labels could not be mapped.")

    return mapped_pred.astype(int)


def get_subject(path):
    parent = os.path.basename(os.path.dirname(path))

    if parent.startswith("S"):
        return parent

    filename = os.path.basename(path)
    return filename.split("_")[0]


def get_trial(path):
    return os.path.basename(path).replace(".arff", "")


# =========================
# Event extraction
# =========================

def estimate_sample_step(time_values):
    time_values = np.asarray(time_values, dtype=float)

    if len(time_values) < 2:
        return 0.0

    diffs = np.diff(time_values)
    diffs = diffs[diffs > 0]

    if len(diffs) == 0:
        return 0.0

    return float(np.median(diffs))


def extract_events(time_values, labels, target_label):
    """
    Extract continuous runs of target_label as events.
    Duration is returned in milliseconds.
    """
    time_values = np.asarray(time_values, dtype=float)
    labels = np.asarray(labels, dtype=int)

    if len(time_values) == 0:
        return []

    sample_step = estimate_sample_step(time_values)

    events = []
    in_event = False
    start_idx = None

    for i, lab in enumerate(labels):
        if lab == target_label and not in_event:
            in_event = True
            start_idx = i

        elif lab != target_label and in_event:
            end_idx = i - 1

            start_time = time_values[start_idx]
            end_time = time_values[end_idx]

            duration = (end_time - start_time + sample_step) / 1000.0
            events.append(duration)

            in_event = False
            start_idx = None

    if in_event:
        end_idx = len(labels) - 1

        start_time = time_values[start_idx]
        end_time = time_values[end_idx]

        duration = (end_time - start_time + sample_step) / 1000.0
        events.append(duration)

    return events


def event_summary(time_values, labels, target_label):
    durations = extract_events(time_values, labels, target_label)

    if len(durations) == 0:
        return {
            "event_count": 0,
            "mean_duration_ms": 0.0,
            "sd_duration_ms": 0.0,
        }

    return {
        "event_count": len(durations),
        "mean_duration_ms": float(np.mean(durations)),
        "sd_duration_ms": float(np.std(durations, ddof=1)) if len(durations) > 1 else 0.0,
    }


def rmse(values):
    values = np.asarray(values, dtype=float)

    if len(values) == 0:
        return np.nan

    return math.sqrt(np.mean(values ** 2))


# =========================
# Main
# =========================

def main():
    files = sorted(glob.glob(os.path.join(OUTPUT_DIR, "**", "*.arff"), recursive=True))

    if not files:
        raise FileNotFoundError(f"No ARFF files found in {OUTPUT_DIR}")

    trial_rows = []
    skipped_trials = []

    for path in files:
        df = read_arff(path)

        if "time" not in df.columns:
            raise ValueError(f"time column not found in {path}")

        if TRUE_COL not in df.columns:
            raise ValueError(f"{TRUE_COL} not found in {path}")

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

        # =========================
        # Important correction:
        # Skip trials with no GT fixation/saccade events
        # =========================

        gt_fix_summary = event_summary(time_values, true_labels, FIX_LABEL)
        gt_sacc_summary = event_summary(time_values, true_labels, SACC_LABEL)

        if (
            gt_fix_summary["event_count"] == 0
            and gt_sacc_summary["event_count"] == 0
        ):
            skipped_trials.append({
                "subject": subject,
                "trial": trial,
                "file": path,
                "reason": "No GT fixation or saccade events"
            })
            print("Skipping trial with no GT fixation/saccade labels:", subject, trial)
            continue

        # =========================
        # Event-level summaries
        # =========================

        for event_name, event_label in [
            ("fixation", FIX_LABEL),
            ("saccade", SACC_LABEL),
        ]:
            gt_summary = event_summary(time_values, true_labels, event_label)
            pred_summary = event_summary(time_values, pred_labels, event_label)

            row = {
                "subject": subject,
                "trial": trial,
                "file": path,
                "event_type": event_name,

                "gt_event_count": gt_summary["event_count"],
                "pred_event_count": pred_summary["event_count"],
                "event_count_error": pred_summary["event_count"] - gt_summary["event_count"],

                "gt_mean_duration_ms": gt_summary["mean_duration_ms"],
                "pred_mean_duration_ms": pred_summary["mean_duration_ms"],
                "mean_duration_error_ms": (
                    pred_summary["mean_duration_ms"] - gt_summary["mean_duration_ms"]
                ),

                "gt_sd_duration_ms": gt_summary["sd_duration_ms"],
                "pred_sd_duration_ms": pred_summary["sd_duration_ms"],
                "sd_duration_error_ms": (
                    pred_summary["sd_duration_ms"] - gt_summary["sd_duration_ms"]
                ),
            }

            trial_rows.append(row)

    trial_df = pd.DataFrame(trial_rows)
    skipped_df = pd.DataFrame(skipped_trials)

    if trial_df.empty:
        raise ValueError("No valid trials remained after skipping empty-GT trials.")

    # =========================
    # Overall RMSE
    # =========================

    overall_rows = []

    for event_type, sub_df in trial_df.groupby("event_type"):
        overall_rows.append({
            "event_type": event_type,
            "n_trials": len(sub_df),

            "gt_total_events": int(sub_df["gt_event_count"].sum()),
            "pred_total_events": int(sub_df["pred_event_count"].sum()),

            "event_count_rmse": rmse(sub_df["event_count_error"]),
            "mean_duration_rmse_ms": rmse(sub_df["mean_duration_error_ms"]),
            "sd_duration_rmse_ms": rmse(sub_df["sd_duration_error_ms"]),

            "mean_event_count_error": float(sub_df["event_count_error"].mean()),
            "mean_duration_error_ms": float(sub_df["mean_duration_error_ms"].mean()),
            "mean_sd_duration_error_ms": float(sub_df["sd_duration_error_ms"].mean()),
        })

    overall_df = pd.DataFrame(overall_rows)

    # =========================
    # By-subject RMSE
    # =========================

    by_subject_rows = []

    for (subject, event_type), sub_df in trial_df.groupby(["subject", "event_type"]):
        by_subject_rows.append({
            "subject": subject,
            "event_type": event_type,
            "n_trials": len(sub_df),

            "gt_total_events": int(sub_df["gt_event_count"].sum()),
            "pred_total_events": int(sub_df["pred_event_count"].sum()),

            "event_count_rmse": rmse(sub_df["event_count_error"]),
            "mean_duration_rmse_ms": rmse(sub_df["mean_duration_error_ms"]),
            "sd_duration_rmse_ms": rmse(sub_df["sd_duration_error_ms"]),

            "mean_event_count_error": float(sub_df["event_count_error"].mean()),
            "mean_duration_error_ms": float(sub_df["mean_duration_error_ms"].mean()),
            "mean_sd_duration_error_ms": float(sub_df["sd_duration_error_ms"].mean()),
        })

    by_subject_df = pd.DataFrame(by_subject_rows)

    # =========================
    # Save result files
    # =========================

    trial_path = os.path.join(
        RESULTS_DIR,
        "ml_speed_direction_event_level_by_trial_valid_gt_only.csv"
    )

    overall_path = os.path.join(
        RESULTS_DIR,
        "ml_speed_direction_event_rmse_overall_valid_gt_only.csv"
    )

    by_subject_path = os.path.join(
        RESULTS_DIR,
        "ml_speed_direction_event_rmse_by_subject_valid_gt_only.csv"
    )

    skipped_path = os.path.join(
        RESULTS_DIR,
        "ml_speed_direction_event_rmse_skipped_trials.csv"
    )

    trial_df.to_csv(trial_path, index=False)
    overall_df.to_csv(overall_path, index=False)
    by_subject_df.to_csv(by_subject_path, index=False)
    skipped_df.to_csv(skipped_path, index=False)

    print("\n==============================")
    print("Event-level RMSE results")
    print("==============================")

    print("\nTotal ARFF files found:", len(files))
    print("Valid trials included:", int(len(trial_df) / 2))
    print("Skipped trials:", len(skipped_df))

    print("\nOverall event-level RMSE:")
    print(overall_df.to_string(index=False))

    print("\nSaved result files:")
    print(trial_path)
    print(overall_path)
    print(by_subject_path)
    print(skipped_path)


if __name__ == "__main__":
    main()