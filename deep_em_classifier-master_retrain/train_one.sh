#!/bin/bash
# Train LOO models for raw / mean_w3 / sg_p3_n7 smoothing (20 subjects each)
# Each subject trained in a separate process to avoid intermittent crashes.
set -u
cd /Users/pci/deep_em_classifier-master

declare -A SMOOTH_MAP=(
  [raw]="raw models_webcam output_raw output_webcam"
  [mean_w3]="mean models_webcam_mean output_mean_w3 output_webcam_mean"
  [sg_p3_n7]="sg models_webcam_sg output_sg_p3_n7 output_webcam_sg"
)

# Feature folder names
declare -A FEAT_MAP=(
  [raw]="output_raw"
  [mean_w3]="output_mean_w3"
  [sg_p3_n7]="output_sg_p3_n7"
)

declare -A MODEL_ROOT_MAP=(
  [raw]="data/models_webcam"
  [mean_w3]="data/models_webcam_mean"
  [sg_p3_n7]="data/models_webcam_sg"
)

declare -A FEAT_DIR_MAP=(
  [raw]="data/inputs/Webcam_features/output_raw"
  [mean_w3]="data/inputs/Webcam_features/output_mean_w3"
  [sg_p3_n7]="data/inputs/Webcam_features/output_sg_p3_n7"
)

SMOOTH="$1"   # raw | mean_w3 | sg_p3_n7
SUBJ="$2"     # e.g. S29

MODEL_DIR="${MODEL_ROOT_MAP[$SMOOTH]}/LOO_4xvC@(32, 16, 8, 8)_0xD@()_2xB@(16, 16)_speed_direction_WINDOW_33_overlap_16"
FEAT_DIR="${FEAT_DIR_MAP[$SMOOTH]}"

# Ensure old (6-subject) empty/stale models don't block; remove any that are 0-byte
for f in "$MODEL_DIR"/Conv_sample_windows_epochs_1000_without_*.h5; do
  [ -f "$f" ] || continue
  if [ ! -s "$f" ]; then
    rm -f "$f"
    echo "Removed empty model: $(basename "$f")"
  fi
done

echo "===== TRAINING $SMOOTH / $SUBJ ====="
/opt/anaconda3/envs/em310/bin/python blstm_model.py \
  --features speed direction --num-conv 4 --conv-units 32 16 8 8 \
  --num-dense 0 --num-blstm 2 --blstm-units 16 16 \
  --window-size 33 --overlap 16 --num-epochs 1000 \
  --training-samples 3000 --batch-size 256 \
  --run-once --run-once-video "$SUBJ" \
  --model-root-path "${MODEL_ROOT_MAP[$SMOOTH]}" \
  --feature-files-folder "$FEAT_DIR/" \
  --sp-tool-folder sp_tool/ 2>&1 | tail -2

f="$MODEL_DIR/Conv_sample_windows_epochs_1000_without_${SUBJ}.h5"
ls -la "$f" | awk '{print $5, $9}'
echo "===== DONE $SMOOTH/$SUBJ ====="
