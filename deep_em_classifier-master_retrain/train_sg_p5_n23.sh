#!/bin/bash
# Train LOO models for SG p=5 n=23 (20 subjects) into a SEPARATE model dir
set -u
cd /Users/pci/deep_em_classifier-master
MODEL_ROOT="data/models_webcam_sg_p5_n23"
FEAT_DIR="data/inputs/Webcam_features/output_sg_p5_n23"
MODEL_DIR="$MODEL_ROOT/LOO_4xvC@(32, 16, 8, 8)_0xD@()_2xB@(16, 16)_speed_direction_WINDOW_33_overlap_16"

for subj in S11 S13 S14 S18 S19 S20 S21 S22 S23 S25 S26 S27 S28 S29 S30 S31 S32 S33 S34 S35; do
  f="$MODEL_DIR/Conv_sample_windows_epochs_1000_without_${subj}.h5"
  if [ -s "$f" ]; then
    echo "SKIP $subj: exists"
    continue
  fi
  echo "===== TRAIN sg_p5_n23/$subj ====="
  /opt/anaconda3/envs/em310/bin/python blstm_model.py \
    --features speed direction --num-conv 4 --conv-units 32 16 8 8 \
    --num-dense 0 --num-blstm 2 --blstm-units 16 16 \
    --window-size 33 --overlap 16 --num-epochs 1000 \
    --training-samples 3000 --batch-size 256 \
    --run-once --run-once-video "$subj" \
    --model-root-path "$MODEL_ROOT" \
    --feature-files-folder "$FEAT_DIR/" \
    --sp-tool-folder sp_tool/ 2>&1 | tail -2
  if [ -s "$f" ]; then
    echo "OK $subj ($(stat -f%z "$f") bytes)"
  else
    echo "FAILED $subj"
    rm -f "$f"
  fi
done
echo "ALL DONE for sg_p5_n23"
