###############################################################################
# manuscript_figures.R
# Generate all 8 main figures + supplementary figures for the manuscript
#
# Figures:
#   Figure 1  – Example trial: webcam+Eyelink signal + velocity + ground truth
#   Figure S1 – Raw signal comparison: EyeLink (1000 Hz) vs Webcam (~60 Hz)
#   Figure 2  – Parameter tuning curves for traditional algorithms (raw data)
#   Figure 3  – Smoothing effects heatmap across algorithms
#   Figure 4  – Best algorithm comparison: sample-level MCC
#   Figure 5  – Wasserstein distance: event distribution similarity
#   Figure 6  – Example prediction: trace + GT vs best traditional vs CNN-BLSTM
#   Figure 7  – Event-duration distributions: GT vs all methods
#   Figure 8  – Saccade amplitude analysis
###############################################################################


# 0. Setup
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(eyemovements)
library(data.table)
library(purrr)
library(T4transport)
library(ggridges)

# Paths
manu_dir  <- "manuscript"
res_dir   <- file.path(manu_dir, "results")
fig_dir   <- file.path(manu_dir, "figures")
data_file <- "data/manual_labels/webdata_manual_labels.csv"

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Theme settings
theme_paper <- theme_classic(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.major  = element_line(linewidth = 0.3, colour = "grey90"),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "grey95", colour = NA),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, colour = "grey40")
  )


# 1. Load data

cat("Loading data...\n")

# Traditional algorithm results
mcc_all  <- read_csv(file.path(res_dir, "all_algorithm_mcc_results.csv"),
                     show_col_types = FALSE)
fix_comp <- read_csv(file.path(res_dir, "event_level_rmse",
                                "fixation_event_level_comparison.csv"),
                     show_col_types = FALSE)
sac_comp <- read_csv(file.path(res_dir, "event_level_rmse",
                                "saccade_event_level_comparison.csv"),
                     show_col_types = FALSE)
fix_ev   <- read_csv(file.path(res_dir, "detected_events",
                                "all_fixation_events.csv"),
                     show_col_types = FALSE)
sac_ev   <- read_csv(file.path(res_dir, "detected_events",
                                "all_saccade_events.csv"),
                     show_col_types = FALSE)

# ML results (CNN-BLSTM with different smoothing)
# 统一口径：CNN-BLSTM 的 sample-level MCC 从 LOO 主模型预测计算
# (blstm_model.py -> data/outputs_webcam*)，与 Figure 5B / 事件级分析一致。
ml_mcc_all <- read_csv("../ml_mcc_loo.csv", show_col_types = FALSE) %>%
  filter(event != "multiclass") %>%
  mutate(method = "CNN-BLSTM",
         parameter_type = "none",
         parameter_value = NA_real_,
         smoothing = case_when(
           smoothing == "sg_p3_n7"  ~ "sg_p3_n7",
           smoothing == "sg_p5_n23" ~ "sg",
           TRUE ~ smoothing
         )) %>%
  select(method, event, smoothing, parameter_type, parameter_value, MCC)

# ML per-trial event-level data (for Wasserstein) - all smoothing types
ml_trial_raw    <- read_csv("../results/ml_event_level_by_trial_raw.csv",    show_col_types = FALSE) %>% mutate(smoothing = "raw")
ml_trial_mean   <- read_csv("../results/ml_event_level_by_trial_mean.csv",   show_col_types = FALSE) %>% mutate(smoothing = "mean")
ml_trial_median <- read_csv("../results/ml_event_level_by_trial_median.csv", show_col_types = FALSE) %>% mutate(smoothing = "median")
ml_trial_sg     <- read_csv("../results/ml_event_level_by_trial_sg.csv",     show_col_types = FALSE) %>% mutate(smoothing = "sg")
ml_trial_sg_p5  <- read_csv("../results/ml_event_level_by_trial_sg_p5_n23.csv", show_col_types = FALSE) %>% mutate(smoothing = "sg_p5_n23")

ml_trial <- bind_rows(ml_trial_raw, ml_trial_mean, ml_trial_median, ml_trial_sg, ml_trial_sg_p5) %>%
  mutate(
    sub = as.numeric(gsub("S", "", subject)),
    method = "CNN-BLSTM",
    parameter = "none"
  )

# Raw data for trial visualization
raw_dat <- read_csv(data_file, show_col_types = FALSE)
raw_dat <- subset(raw_dat, conf > 0 & el_pupil > 0)


# Helper: best MCC row

get_best_mcc <- function(df, event_type) {
  df %>%
    filter(event == event_type) %>%
    group_by(method, smoothing) %>%
    slice_max(MCC, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    slice_max(MCC, n = 1, with_ties = FALSE)
}

best_fix_trad <- lapply(c("IVT", "IDT", "EK03", "IHMM"), function(m) {
  get_best_mcc(mcc_all %>% filter(method == m), "fixation")
}) %>% bind_rows()

best_sac_trad <- lapply(c("IVT", "IDT", "EK03", "IHMM"), function(m) {
  get_best_mcc(mcc_all %>% filter(method == m), "saccade")
}) %>% bind_rows()

best_fix_ml <- get_best_mcc(ml_mcc_all, "fixation")
best_sac_ml <- get_best_mcc(ml_mcc_all, "saccade")

best_all <- bind_rows(
  best_fix_trad %>% mutate(best_for_event = "fixation"),
  best_sac_trad %>% mutate(best_for_event = "saccade"),
  best_fix_ml   %>% mutate(best_for_event = "fixation"),
  best_sac_ml   %>% mutate(best_for_event = "saccade")
)

cat("\n=== Best parameters per method ===\n")
print(best_all %>% select(method, best_for_event, smoothing, parameter_value, MCC))


# Figure 1: Example trial - webcam+Eyelink signal + velocity + ground truth

cat("\nGenerating Figure 1...\n")

fig1_sub   <- 11
fig1_trial <- 114

d1 <- raw_dat %>%
  filter(sub == fig1_sub, Trial_Id == fig1_trial) %>%
  transmute(
    time = time_start,
    x = x, y = y,
    el_x = el_x, el_y = el_y,
    GT = ground_truth
  ) %>%
  arrange(time) %>%
  mutate(
    dx = c(NA, diff(x)) * 0.01865554,
    dy = c(NA, diff(y)) * 0.01919689,
    dt = c(NA, diff(time)),
    vel = sqrt(dx^2 + dy^2) / (dt / 1000),
    el_dx = c(NA, diff(el_x)) * 0.01865554,
    el_dy = c(NA, diff(el_y)) * 0.01919689,
    el_vel = sqrt(el_dx^2 + el_dy^2) / (dt / 1000)
  )

# Trace: Webcam + EyeLink
P1_trace <- bind_rows(
  d1 %>% select(time, x, y) %>%
    pivot_longer(c(x, y), names_to = "signal", values_to = "position") %>%
    mutate(tracker = "Webcam"),
  d1 %>% select(time, el_x, el_y) %>%
    rename(x = el_x, y = el_y) %>%
    pivot_longer(c(x, y), names_to = "signal", values_to = "position") %>%
    mutate(tracker = "EyeLink")
) %>%
  mutate(signal = factor(signal, levels = c("x", "y"),
                         labels = c("X position", "Y position"))) %>%
  ggplot(aes(x = time, y = position, colour = signal, linetype = tracker)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c("X position" = "#E41A1C",
                                  "Y position" = "#377EB8")) +
  scale_linetype_manual(values = c(Webcam = "solid", EyeLink = "dashed")) +
  labs(title = "A. Gaze position (Webcam + EyeLink)", x = NULL,
       y = "Position (px)", colour = NULL, linetype = NULL) +
  theme_paper

P1_vel <- bind_rows(
  d1 %>% select(time, vel) %>% mutate(source = "Webcam"),
  d1 %>% select(time, el_vel) %>% rename(vel = el_vel) %>% mutate(source = "EyeLink")
) %>%
  ggplot(aes(x = time, y = vel, colour = source, linetype = source)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c(Webcam = "#4DAF4A", EyeLink = "#E41A1C")) +
  scale_linetype_manual(values = c(Webcam = "solid", EyeLink = "dashed")) +
  labs(title = "B. Gaze velocity (Webcam + EyeLink)", x = NULL, y = "Velocity (deg/s)",
       colour = NULL, linetype = NULL) +
  theme_paper

P1_gt <- d1 %>%
  mutate(GT = factor(GT, levels = c("fixation", "saccade", "pso", "blink", "unclear"))) %>%
  ggplot(aes(x = time, y = 1, fill = GT)) +
  geom_tile() +
  scale_fill_manual(
    values = c(fixation = "#66C2A5", saccade = "#FC8D62",
               pso = "#8DA0CB", blink = "#E78AC3", unclear = "#A6D854"),
    drop = TRUE
  ) +
  labs(title = "C. Ground truth labels", x = "Time (ms)", y = NULL, fill = NULL) +
  theme_paper +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

fig1 <- P1_trace / P1_vel / P1_gt +
  plot_layout(heights = c(2, 2, 0.8))

ggsave(file.path(fig_dir, "Figure1_example_trial.pdf"),
       fig1, width = 10, height = 7, device = "pdf")
ggsave(file.path(fig_dir, "Figure1_example_trial.png"),
       fig1, width = 10, height = 7, dpi = 300)
cat("  Figure 1 saved.\n")

# ===========================================================================
# Figure S1: Raw signal comparison - EyeLink (1000 Hz) vs Webcam (~60 Hz)
# Same 500 ms segment of trial 114 (subject 11), aligned on EyeLink time.
# Contrasts sampling density (500 vs ~26 samples) and noise level, with a
# fixation -> saccade -> fixation sequence clearly visible in EyeLink.
# ===========================================================================
cat("Generating Figure S1 (raw signal comparison)...\n")

el_raw <- read_csv("data/manual_labels/raw_sample_data/sub11.csv",
                   show_col_types = FALSE)

t0 <- 12404850  # 500 ms window start (EyeLink ms)

el_win <- el_raw %>%
  filter(item == 14, el_time >= t0, el_time <= t0 + 500) %>%
  transmute(time_ms = el_time - t0, x = x, tracker = "EyeLink (1000 Hz)")

wc_win <- raw_dat %>%
  filter(sub == fig1_sub, Trial_Id == fig1_trial,
         el_time >= t0, el_time <= t0 + 500) %>%
  transmute(time_ms = el_time - t0, x = x, tracker = "Webcam (~60 Hz)")

sig_dat <- bind_rows(el_win, wc_win) %>%
  mutate(tracker = factor(tracker,
                          levels = c("EyeLink (1000 Hz)", "Webcam (~60 Hz)")))

n_el  <- nrow(el_win)
n_wc  <- nrow(wc_win)
sac_r <- data.frame(xmin = 12405073 - t0, xmax = 12405151 - t0)

cnt_dat <- data.frame(
  tracker = factor(c("EyeLink (1000 Hz)", "Webcam (~60 Hz)"),
                   levels = c("EyeLink (1000 Hz)", "Webcam (~60 Hz)")),
  label   = sprintf("N = %d samples", c(n_el, n_wc)),
  time_ms = 15, y = Inf
)

P_sig <- ggplot(sig_dat, aes(x = time_ms, y = x)) +
  geom_rect(data = sac_r, aes(xmin = xmin, xmax = xmax), ymin = -Inf, ymax = Inf,
            inherit.aes = FALSE, fill = "#FC8D62", alpha = 0.28) +
  geom_line(linewidth = 0.45, colour = "#333333") +
  geom_point(size = 0.9, colour = "#333333") +
  geom_text(data = cnt_dat, aes(x = time_ms, y = y, label = label),
            hjust = 0, vjust = 1.1, size = 4, fontface = "bold",
            colour = "grey15", inherit.aes = FALSE) +
  annotate("text", x = 100, y = Inf, label = "fixation", vjust = 2.6,
           colour = "grey30", size = 3.8, fontface = "italic") +
  annotate("text", x = 262, y = Inf, label = "saccade", vjust = 2.6,
           colour = "#D94801", size = 3.8, fontface = "italic") +
  annotate("text", x = 405, y = Inf, label = "fixation", vjust = 2.6,
           colour = "grey30", size = 3.8, fontface = "italic") +
  facet_wrap(~ tracker, ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  labs(title = "Raw gaze signal: EyeLink (1000 Hz) vs Webcam",
       subtitle = "Same 500 ms segment of trial 114 (subject 11).",
       x = "Time within segment (ms)", y = "X position (px)") +
  theme_paper

ggsave(file.path(fig_dir, "FigureS1_raw_signal_comparison.pdf"),
       P_sig, width = 9, height = 6.5, device = "pdf")
ggsave(file.path(fig_dir, "FigureS1_raw_signal_comparison.png"),
       P_sig, width = 9, height = 6.5, dpi = 300)
cat(sprintf("  Figure S1 saved (EyeLink=%d, Webcam=%d samples).\n", n_el, n_wc))

# ===========================================================================
# Figure 2: Parameter tuning curves (raw smoothing only, no IHMM)
# ===========================================================================
cat("Generating Figure 2...\n")

tune_dat <- mcc_all %>%
  filter(smoothing == "raw", method != "IHMM") %>%
  mutate(
    panel = case_when(
      method == "IVT"  ~ "A. IVT (velocity threshold)",
      method == "IDT"  ~ "B. IDT (dispersion threshold)",
      method == "EK03" ~ "C. EK03 (lambda)"
    ),
    panel = factor(panel, levels = c(
      "A. IVT (velocity threshold)",
      "B. IDT (dispersion threshold)",
      "C. EK03 (lambda)"
    ))
  )

fig2 <- ggplot(tune_dat, aes(x = parameter_value, y = MCC,
                              colour = event, shape = event)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_colour_manual(values = c(fixation = "#1B9E77", saccade = "#D95F02"),
                      labels = c(fixation = "Fixation MCC", saccade = "Saccade MCC")) +
  scale_shape_manual(values = c(fixation = 16, saccade = 17),
                     labels = c(fixation = "Fixation MCC", saccade = "Saccade MCC")) +
  labs(title = "Parameter tuning curves (raw, unsmoothed data)",
       x = "Parameter value", y = "MCC", colour = NULL, shape = NULL) +
  theme_paper

ggsave(file.path(fig_dir, "Figure2_parameter_tuning.pdf"),
       fig2, width = 10, height = 5, device = "pdf")
ggsave(file.path(fig_dir, "Figure2_parameter_tuning.png"),
       fig2, width = 10, height = 5, dpi = 300)
cat("  Figure 2 saved.\n")

# ===========================================================================
# Figure 3: Smoothing effects heatmap
# ===========================================================================
cat("Generating Figure 3...\n")

heat_dat <- mcc_all %>%
  group_by(method, event, smoothing) %>%
  summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop")

ml_heat <- ml_mcc_all %>%
  group_by(method, event, smoothing) %>%
  summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop")

heat_dat <- bind_rows(heat_dat, ml_heat) %>%
  mutate(
    method = factor(method,
                    levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM")),
    smoothing = factor(smoothing,
                       levels = c("raw", "mean", "median", "sg_p3_n7", "sg"),
                       labels = c("Raw", "Mean w=3", "Median w=3",
                                  "SG p=3 n=7", "SG p=5 n=23")),
    event = factor(event, levels = c("fixation", "saccade"))
  )

fig3a <- heat_dat %>%
  filter(event == "fixation", method != "CNN-BLSTM") %>%
  mutate(method = droplevels(method)) %>%
  ggplot(aes(x = smoothing, y = method, fill = best_MCC)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", best_MCC)), size = 5) +
  scale_fill_gradientn(colours = c("white", "#FFF7BC", "#FEE391", "#FEC44F",
                                    "#FE9929", "#EC7014", "#CC4C02", "#8C2D04"),
                       limits = c(0, 0.6), na.value = "grey90", name = "MCC") +
  labs(title = "A. Fixation MCC", x = "Smoothing method", y = NULL) +
  theme_paper + theme(axis.text.x = element_text(angle = 30, hjust = 1))

fig3b <- heat_dat %>%
  filter(event == "saccade", method != "CNN-BLSTM") %>%
  mutate(method = droplevels(method)) %>%
  ggplot(aes(x = smoothing, y = method, fill = best_MCC)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", best_MCC)), size = 5) +
  scale_fill_gradientn(colours = c("white", "#FFF7BC", "#FEE391", "#FEC44F",
                                    "#FE9929", "#EC7014", "#CC4C02", "#8C2D04"),
                       limits = c(0, 0.6), na.value = "grey90", name = "MCC") +
  labs(title = "B. Saccade MCC", x = "Smoothing method", y = NULL) +
  theme_paper + theme(axis.text.x = element_text(angle = 30, hjust = 1))

fig3 <- fig3a + fig3b + plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "Figure3_smoothing_heatmap.pdf"),
       fig3, width = 12, height = 5, device = "pdf")
ggsave(file.path(fig_dir, "Figure3_smoothing_heatmap.png"),
       fig3, width = 12, height = 5, dpi = 300)
cat("  Figure 3 saved.\n")

# ===========================================================================
# Figure 4: Best algorithm comparison - sample-level MCC
# ===========================================================================
cat("Generating Figure 4...\n")

fig4_dat <- bind_rows(
  mcc_all %>%
    group_by(method, event) %>%
    summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop"),
  ml_mcc_all %>%
    group_by(method, event) %>%
    summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop")
) %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM")),
    event  = factor(event, levels = c("fixation", "saccade"))
  )

fig4 <- ggplot(fig4_dat, aes(x = method, y = best_MCC, fill = event)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", best_MCC)),
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(fixation = "#1B9E77", saccade = "#D95F02"),
                    labels = c(fixation = "Fixation MCC", saccade = "Saccade MCC")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, NA)) +
  labs(title = "Best algorithm comparison: sample-level MCC",
       subtitle = "Each algorithm uses its best smoothing method and parameters",
       x = NULL, y = "MCC", fill = NULL) +
  theme_paper

ggsave(file.path(fig_dir, "Figure4_best_comparison.pdf"),
       fig4, width = 8, height = 5, device = "pdf")
ggsave(file.path(fig_dir, "Figure4_best_comparison.png"),
       fig4, width = 8, height = 5, dpi = 300)
cat("  Figure 4 saved.\n")

# ===========================================================================
# Figure 4b: Best fixation MCC - traditional algorithms only (focused)
# ===========================================================================
cat("Generating Figure 4b...\n")

fig4b_dat <- mcc_all %>%
  filter(event == "fixation") %>%
  group_by(method) %>%
  summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM")),
    label = sprintf("%.3f", best_MCC)
  ) %>%
  arrange(desc(best_MCC))

fig4b <- ggplot(fig4b_dat, aes(x = method, y = best_MCC, fill = method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), vjust = -0.3, size = 4.5) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, NA)) +
  labs(title = "Best fixation MCC: traditional algorithms",
       subtitle = "Each algorithm uses its best smoothing method and parameters",
       x = NULL, y = "Fixation MCC") +
  theme_paper

ggsave(file.path(fig_dir, "Figure4b_best_fixation_traditional.pdf"),
       fig4b, width = 6, height = 4.5, device = "pdf")
ggsave(file.path(fig_dir, "Figure4b_best_fixation_traditional.png"),
       fig4b, width = 6, height = 4.5, dpi = 300)
cat("  Figure 4b saved.\n")

# ===========================================================================
# Figure 4c: Best algorithm comparison - traditional only (no CNN-BLSTM)
# ===========================================================================
cat("Generating Figure 4c...\n")

fig4c_dat <- mcc_all %>%
  group_by(method, event) %>%
  summarise(best_MCC = max(MCC, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM")),
    event  = factor(event, levels = c("fixation", "saccade"))
  )

fig4c <- ggplot(fig4c_dat, aes(x = method, y = best_MCC, fill = event)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", best_MCC)),
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(fixation = "#1B9E77", saccade = "#D95F02"),
                    labels = c(fixation = "Fixation MCC", saccade = "Saccade MCC")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, NA)) +
  labs(title = "Best algorithm comparison: traditional algorithms",
       subtitle = "Each algorithm uses its best smoothing method and parameters",
       x = NULL, y = "MCC", fill = NULL) +
  theme_paper

ggsave(file.path(fig_dir, "Figure4c_best_comparison_traditional.pdf"),
       fig4c, width = 8, height = 5, device = "pdf")
ggsave(file.path(fig_dir, "Figure4c_best_comparison_traditional.png"),
       fig4c, width = 8, height = 5, dpi = 300)
cat("  Figure 4c saved.\n")

# ===========================================================================
# Figure 5: Combined MCC + Wasserstein heatmaps (sample-level + event-level)
# ===========================================================================
cat("Generating Figure 5 (MCC + Wasserstein combined)...\n")

# === Wasserstein helper (multivariate + univariate) ===
compute_wasserstein_full <- function(gt_mat, algo_mat, p = 1) {
  valid <- complete.cases(gt_mat) & complete.cases(algo_mat)
  gt_mat   <- gt_mat[valid, , drop = FALSE]
  algo_mat <- algo_mat[valid, , drop = FALSE]
  if (nrow(gt_mat) < 3 || nrow(algo_mat) < 3)
    return(list(W_multi = NA, W_n = NA, W_mean = NA, W_sd = NA))

  gt_center <- colMeans(gt_mat)
  gt_scale  <- apply(gt_mat, 2, sd)
  gt_scale[gt_scale == 0] <- 1

  gt_scaled   <- scale(gt_mat,   center = gt_center, scale = gt_scale)
  algo_scaled <- scale(algo_mat, center = gt_center, scale = gt_scale)

  w_dist <- function(g, a, cols) {
    d <- T4transport::wasserstein(
      scale(g[, cols, drop = FALSE], center = gt_center[cols], scale = gt_scale[cols]),
      scale(a[, cols, drop = FALSE], center = gt_center[cols], scale = gt_scale[cols]),
      p = p
    )$distance
    1 / (1 + d)
  }

  list(
    W_multi = 1 / (1 + T4transport::wasserstein(gt_scaled, algo_scaled, p = p)$distance),
    W_n     = w_dist(gt_mat, algo_mat, 1),
    W_mean  = w_dist(gt_mat, algo_mat, 2),
    W_sd    = w_dist(gt_mat, algo_mat, 3)
  )
}

run_wasserstein_full <- function(comp_df, event_type) {
  n_col  <- if (event_type == "fixation") "algo_n_fixations"  else "algo_n_saccades"
  m_col  <- if (event_type == "fixation") "algo_mean_fix_dur" else "algo_mean_sacc_dur"
  s_col  <- if (event_type == "fixation") "algo_sd_fix_dur"   else "algo_sd_sacc_dur"
  gn_col <- if (event_type == "fixation") "GT_n_fixations"    else "GT_n_saccades"
  gm_col <- if (event_type == "fixation") "GT_mean_fix_dur"   else "GT_mean_sacc_dur"
  gs_col <- if (event_type == "fixation") "GT_sd_fix_dur"     else "GT_sd_sacc_dur"

  comp_df %>%
    group_by(method, smoothing, parameter) %>%
    group_modify(~ {
      gt   <- .x %>% select(all_of(c(gn_col, gm_col, gs_col))) %>% as.matrix()
      algo <- .x %>% select(all_of(c(n_col,  m_col,  s_col)))  %>% as.matrix()
      w <- compute_wasserstein_full(gt, algo)
      data.frame(
        W_multi = w$W_multi, W_n = w$W_n,
        W_mean = w$W_mean, W_sd = w$W_sd
      )
    }) %>%
    ungroup() %>%
    mutate(event = event_type)
}

# === Traditional algorithms ===
W_fix <- run_wasserstein_full(fix_comp, "fixation")
W_sac <- run_wasserstein_full(sac_comp, "saccade")

# === CNN-BLSTM Wasserstein (from per-trial data) ===
ml_wasserstein <- ml_trial %>%
  group_by(method, smoothing, parameter, event_type) %>%
  group_modify(~ {
    gt <- .x %>%
      select(gt_event_count, gt_mean_duration_ms, gt_sd_duration_ms) %>%
      as.matrix()
    algo <- .x %>%
      select(pred_event_count, pred_mean_duration_ms, pred_sd_duration_ms) %>%
      as.matrix()
    w <- compute_wasserstein_full(gt, algo)
    data.frame(
      W_multi = w$W_multi, W_n = w$W_n,
      W_mean = w$W_mean, W_sd = w$W_sd
    )
  }) %>%
  ungroup() %>%
  rename(event = event_type)

W_all <- bind_rows(W_fix, W_sac, ml_wasserstein)

# Best W_score per method+event+smoothing (raw names first, labels later)
W_best <- W_all %>%
  filter(!is.na(W_multi)) %>%
  group_by(method, event, smoothing) %>%
  slice_max(W_multi, n = 1, with_ties = FALSE) %>%
  ungroup()

# CNN-BLSTM: map smoothing names to match heatmap columns
# CNN-BLSTM "sg" = SG p=3 n=7 ; "sg_p5_n23" = SG p=5 n=23
smooth_levels <- c("raw", "mean", "median", "sg_p3_n7", "sg")

W_best <- W_best %>%
  mutate(
    smoothing = case_when(
      method == "CNN-BLSTM" & smoothing == "sg" ~ "sg_p3_n7",
      method == "CNN-BLSTM" & smoothing == "sg_p5_n23" ~ "sg",
      TRUE ~ smoothing
    )
  )

# Now apply factor labels for display
smooth_labels <- c("Raw", "Mean w=3", "Median w=3", "SG p=3 n=7", "SG p=5 n=23")
W_best <- W_best %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM")),
    smoothing = factor(smoothing, levels = smooth_levels, labels = smooth_labels),
    event = factor(event, levels = c("fixation", "saccade"))
  )

# === Panel A: MCC heatmap (from Figure 3 data) ===
fig5a <- heat_dat %>%
  ggplot(aes(x = smoothing, y = method, fill = best_MCC)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", best_MCC)), size = 3.2) +
  facet_wrap(~ event) +
  scale_fill_gradientn(colours = c("white", "#FFF7BC", "#FEE391", "#FEC44F",
                                    "#FE9929", "#EC7014", "#CC4C02", "#8C2D04"),
                       limits = c(0, 0.7), na.value = "grey90", name = "MCC") +
  labs(title = "A. Sample-level agreement (MCC)", x = NULL, y = NULL) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right")

# === Panel B: Wasserstein heatmap ===
fig5b <- W_best %>%
  ggplot(aes(x = smoothing, y = method, fill = W_multi)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", W_multi)), size = 3.2) +
  facet_wrap(~ event) +
  scale_fill_gradientn(colours = c("white", "#D4E6F1", "#85C1E9", "#3498DB",
                                    "#2E86C1", "#1B4F72"),
                       limits = c(0, 1), na.value = "grey90",
                       name = "Wasserstein\nScore") +
  labs(title = "B. Event-level agreement (Wasserstein)",
       x = "Smoothing method", y = NULL) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right")

fig5 <- fig5a / fig5b +
  plot_layout(guides = "collect") &
  theme(legend.position = "right", legend.key.height = unit(1.2, "cm"))

ggsave(file.path(fig_dir, "Figure5_combined_heatmap.pdf"),
       fig5, width = 13, height = 8, device = "pdf")
ggsave(file.path(fig_dir, "Figure5_combined_heatmap.png"),
       fig5, width = 13, height = 8, dpi = 300)
cat("  Figure 5 saved.\n")

# === Table S2: Wasserstein component scores (best per method) ===
cat("Generating Table S2 (Wasserstein components)...\n")

table_s2 <- W_all %>%
  filter(!is.na(W_multi)) %>%
  group_by(method, event) %>%
  slice_max(W_multi, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(method, event, smoothing,
         `W_multi (overall)` = W_multi,
         `W_n (count)` = W_n,
         `W_mean (duration)` = W_mean,
         `W_sd (SD)` = W_sd) %>%
  arrange(event, desc(`W_multi (overall)`))

print(table_s2)
write_csv(table_s2, file.path(fig_dir, "TableS2_wasserstein_components.csv"))
cat("  Table S2 saved.\n")

# ===========================================================================
# Figure 6: Example prediction - trace + GT vs best traditional vs CNN-BLSTM
# ===========================================================================
cat("Generating Figure 6...\n")

fig6_sub   <- 11
fig6_trial <- 114

d6 <- raw_dat %>%
  filter(sub == fig6_sub, Trial_Id == fig6_trial) %>%
  transmute(
    x = x, y = y, time = time_start, GT = ground_truth,
    el_x = el_x, el_y = el_y
  )

d6_sm <- SmoothSamples(d6 %>% select(time, x, y, GT),
                       method = "Mean", window_size = 3)

# Run best algorithms
ivt_best <- best_all %>%
  filter(method == "IVT", best_for_event == "fixation") %>% slice(1)
algo_IVT <- IVT(data = d6_sm, vel_threshold = ivt_best$parameter_value,
                min_fix_dur = 50, return_saccades = FALSE,
                dva_x = 0.01865554, dva_y = 0.01919689)

idt_best <- best_all %>%
  filter(method == "IDT", best_for_event == "fixation") %>% slice(1)
algo_IDT <- IDT(data = d6_sm, disp_threshold = idt_best$parameter_value,
                window_threshold = 50, return_saccades = FALSE,
                dva_x = 0.01865554, dva_y = 0.01919689)

ek_best <- best_all %>%
  filter(method == "EK03", best_for_event == "fixation") %>% slice(1)
algo_EK <- EngbertKliegl03(data = d6_sm, lambda = ek_best$parameter_value,
                           min_fix_dur = 50, return_saccades = FALSE,
                           dva_x = 0.01865554, dva_y = 0.01919689)

algo_IHMM <- IHMM(data = d6_sm, min_fix_dur = 50, return_saccades = FALSE,
                  dva_x = 0.01865554, dva_y = 0.01919689)

# Convert events to sample-level states
events_to_samples <- function(algo_df, col_name) {
  d6_sm %>%
    left_join(
      algo_df %>% transmute(fix_start, fix_end, state = "fixation"),
      join_by(between(time, fix_start, fix_end))
    ) %>%
    mutate(state = ifelse(is.na(state), "none", state)) %>%
    select(time, !!col_name := state)
}

d_IVT  <- events_to_samples(algo_IVT, "IVT")
d_IDT  <- events_to_samples(algo_IDT, "IDT")
d_EK   <- events_to_samples(algo_EK, "EK")
d_IHMM <- events_to_samples(algo_IHMM, "IHMM")

# Build wide prediction table
pred_seq <- d6_sm %>%
  select(time, GT) %>%
  left_join(d_IVT, by = "time") %>%
  left_join(d_IDT, by = "time") %>%
  left_join(d_EK, by = "time") %>%
  left_join(d_IHMM, by = "time")

# Load CNN-BLSTM predictions from ARFF
arff_file <- sprintf("../data/outputs_webcam_median/S%d/S%d_E0I%dD0.arff",
                     fig6_sub, fig6_sub, fig6_trial)
if (file.exists(arff_file)) {
  arff_lines <- readLines(arff_file)
  data_start <- which(arff_lines == "@DATA") + 1
  ml_pred <- read.csv(text = arff_lines[data_start:length(arff_lines)],
                      header = FALSE,
                      col.names = c("time", "x", "y", "conf",
                                    "hl", "EYE_MOVEMENT_TYPE")) %>%
    transmute(
      time = time / 1000,
      CNN_BLSTM = ifelse(EYE_MOVEMENT_TYPE == "FIX", "fixation", "none")
    )
  # Nearest-time matching: assign each smoothed sample the label of the
  # closest CNN-BLSTM prediction sample (both are in ms after /1000).
  nearest_idx <- vapply(pred_seq$time,
                        function(t) which.min(abs(ml_pred$time - t)),
                        integer(1))
  pred_seq$CNN_BLSTM <- ml_pred$CNN_BLSTM[nearest_idx]
  cat("  CNN-BLSTM predictions loaded.\n")
} else {
  pred_seq <- pred_seq %>% mutate(CNN_BLSTM = NA_character_)
  cat("  CNN-BLSTM ARFF not found, skipping.\n")
}

# Pivot to long format
pred_seq <- pred_seq %>%
  pivot_longer(c(GT, IVT, IDT, EK, IHMM, CNN_BLSTM),
               names_to = "source", values_to = "state") %>%
  mutate(
    source = recode(source, GT = "Ground truth",
                    CNN_BLSTM = "CNN-BLSTM"),
    source = factor(source,
                    levels = c("CNN-BLSTM", "IHMM", "EK",
                               "IDT", "IVT", "Ground truth")),
    state = ifelse(state == "fixation", "fixation", "non-fixation")
  ) %>%
  filter(!is.na(state))

# Convert to event rectangles
event_tiles <- pred_seq %>%
  arrange(source, time) %>%
  group_by(source) %>%
  mutate(event_id = data.table::rleid(state)) %>%
  group_by(source, event_id, state) %>%
  summarise(start = min(time), end = max(time), .groups = "drop") %>%
  filter(state == "fixation") %>%
  mutate(
    source_num = as.numeric(source),
    ymin = source_num - 0.35,
    ymax = source_num + 0.35
  )

# Trace plot: Webcam + EyeLink
P6_trace <- bind_rows(
  d6_sm %>%
    select(time, x, y) %>%
    pivot_longer(c(x, y), names_to = "signal", values_to = "position") %>%
    mutate(tracker = "Webcam"),
  d6 %>%
    select(time, el_x, el_y) %>%
    rename(x = el_x, y = el_y) %>%
    pivot_longer(c(x, y), names_to = "signal", values_to = "position") %>%
    mutate(tracker = "EyeLink")
) %>%
  mutate(signal = factor(signal, levels = c("x", "y"),
                         labels = c("X position", "Y position"))) %>%
  ggplot(aes(x = time, y = position, colour = signal, linetype = tracker)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c("X position" = "#E41A1C",
                                  "Y position" = "#377EB8")) +
  scale_linetype_manual(values = c(Webcam = "solid", EyeLink = "dashed")) +
  labs(title = "Gaze position (Webcam + EyeLink) \u2013 Sub 11, Trial 114",
       x = NULL, y = "Position (px)", colour = NULL, linetype = NULL) +
  theme_paper +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Event sequence plot
P6_events <- ggplot(event_tiles) +
  geom_rect(aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax,
                fill = source),
            colour = "black", linewidth = 0.2, alpha = 0.85) +
  scale_y_continuous(
    breaks = seq_along(levels(event_tiles$source)),
    labels = levels(event_tiles$source),
    limits = c(0.5, length(levels(event_tiles$source)) + 0.5)
  ) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(title = "Fixation prediction: Ground truth vs best algorithms",
       x = "Trial time (ms)", y = NULL) +
  theme_paper +
  theme(axis.text.y = element_text(face = "bold"))

fig6 <- P6_trace / P6_events +
  plot_layout(heights = c(2, 1.5))

ggsave(file.path(fig_dir, "Figure6_prediction_sequence.pdf"),
       fig6, width = 10, height = 5.5, device = "pdf")
ggsave(file.path(fig_dir, "Figure6_prediction_sequence.png"),
       fig6, width = 10, height = 5.5, dpi = 300)
cat("  Figure 6 saved.\n")

# ===========================================================================
# Figure 7: Event-duration distributions - GT vs all methods (fixation & saccade)
# ===========================================================================
cat("Generating Figure 7...\n")

# Parse CNN-BLSTM output ARFF files. GT labels are in 'handlabeller_final'
# (1 = fixation, 2 = saccade); predictions are in 'EYE_MOVEMENT_TYPE'.
# Duration = (end - start + sample_step) / 1000 ms, matching Event_Level.py.
parse_arff_durations <- function(dir) {
  files <- list.files(dir, pattern = "\\.arff$", recursive = TRUE, full.names = TRUE)
  gt_fix <- gt_sac <- pred_fix <- pred_sac <- numeric(0)
  for (f in files) {
    d <- fread(f, skip = "@DATA", header = FALSE, showProgress = FALSE, na.strings = character())
    if (nrow(d) == 0) next
    time <- as.numeric(d[[1]])
    gt   <- as.integer(d[[5]])
    pred <- trimws(as.character(d[[6]]))
    diffs <- diff(time); diffs <- diffs[diffs > 0]
    step  <- if (length(diffs) == 0) 0 else median(diffs)
    runs <- function(is_target) {
      r <- rle(is_target)
      ends <- cumsum(r$lengths)
      starts <- c(1, ends[-length(ends)] + 1)
      (time[ends[r$values]] - time[starts[r$values]] + step) / 1000
    }
    gt_fix   <- c(gt_fix,   runs(gt == 1))
    gt_sac   <- c(gt_sac,   runs(gt == 2))
    pred_fix <- c(pred_fix, runs(pred %in% c("FIX", "FIXATION")))
    pred_sac <- c(pred_sac, runs(pred %in% c("SACCADE", "SACC")))
  }
  list(gt_fix = gt_fix, gt_sac = gt_sac, pred_fix = pred_fix, pred_sac = pred_sac)
}

# GT (identical across smoothing) + CNN-BLSTM fixation from mean-smoothed outputs;
# CNN-BLSTM saccade from Savitzky-Golay (p=5, n=23) outputs (best smoothing each).
ml_mean <- parse_arff_durations("../data/outputs_webcam_mean")
ml_sg   <- parse_arff_durations("../data/outputs_webcam_sg_p5_n23")

# Best-model durations from detected events (traditional algorithms)
best_trad_params <- mcc_all %>%
  group_by(method, event) %>%
  slice_max(MCC, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(param_str = case_when(
    method == "IVT"  ~ paste0("vel_", parameter_value),
    method == "IDT"  ~ paste0("disp_", parameter_value),
    method == "EK03" ~ paste0("lambda_", parameter_value),
    method == "IHMM" ~ "none"
  ))

get_best_events <- function(method_name, event_type) {
  b  <- best_trad_params %>% filter(method == method_name, event == event_type)
  ev <- if (event_type == "fixation") fix_ev else sac_ev
  ev %>%
    filter(method == method_name, smoothing == b$smoothing, parameter == b$param_str) %>%
    mutate(event = event_type) %>%
    select(method, event, duration)
}

best_trad_events <- bind_rows(
  lapply(unique(best_trad_params$method), function(m) {
    bind_rows(get_best_events(m, "fixation"), get_best_events(m, "saccade"))
  })
)

fig7_dat <- bind_rows(
  data.frame(method = "Ground truth", event = "fixation", duration = ml_mean$gt_fix),
  data.frame(method = "Ground truth", event = "saccade",  duration = ml_mean$gt_sac),
  best_trad_events,
  data.frame(method = "CNN-BLSTM", event = "fixation", duration = ml_mean$pred_fix),
  data.frame(method = "CNN-BLSTM", event = "saccade",  duration = ml_sg$pred_sac)
) %>%
  filter(duration > 0, duration < 2000) %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM", "Ground truth")),
    event  = factor(event, levels = c("fixation", "saccade"),
                    labels = c("A. Fixation duration", "B. Saccade duration"))
  )

method_cols <- c(IVT = "#E41A1C", IDT = "#377EB8", EK03 = "#4DAF4A",
                 IHMM = "#984EA3", `CNN-BLSTM` = "#FF7F00", `Ground truth` = "#444444")

# 山脊图：每个方法一行密度曲线，GT 置顶
fig7_dat <- fig7_dat %>%
  mutate(method = factor(method,
                         levels = rev(c("IVT", "IDT", "EK03", "IHMM",
                                        "CNN-BLSTM", "Ground truth"))))

# 每个 event x method 的均值（竖虚线）
fig7_means <- fig7_dat %>%
  group_by(event, method) %>%
  summarise(mean_dur = mean(duration), .groups = "drop") %>%
  mutate(method_num = as.numeric(method))

fig7 <- ggplot(fig7_dat, aes(x = duration, y = method, fill = method)) +
  geom_density_ridges(alpha = 0.75, scale = 0.88, rel_min_height = 0.005,
                      colour = "grey25", linewidth = 0.3) +
  geom_segment(data = fig7_means,
               aes(x = mean_dur, xend = mean_dur,
                   y = method_num - 0.28, yend = method_num + 0.28),
               inherit.aes = FALSE, colour = "black",
               linetype = "dashed", linewidth = 0.45) +
  coord_cartesian(ylim = c(0.5, 6.5), clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  facet_wrap(~ event, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = method_cols, name = NULL) +
  labs(title = "Event-duration distributions across event-detection methods",
       x = "Duration (ms)", y = NULL) +
  theme_paper + theme(legend.position = "right")

ggsave(file.path(fig_dir, "Figure7_duration_density.pdf"),
       fig7, width = 12, height = 11, device = "pdf")
ggsave(file.path(fig_dir, "Figure7_duration_density.png"),
       fig7, width = 12, height = 11, dpi = 300)
cat("  Figure 7 saved.\n")

# ===========================================================================
# Figure 8: Saccade amplitude analysis
# ===========================================================================
cat("Generating Figure 8...\n")

# Compute saccade amplitude in degrees
sac_amplitude <- sac_ev %>%
  left_join(
    raw_dat %>% select(sub, Trial_Id, time = time_start, x, y),
    by = c("sub", "Trial_Id"),
    relationship = "many-to-many"
  ) %>%
  filter(time >= sacc_start, time <= sacc_end) %>%
  group_by(method, smoothing, parameter, sub, Trial_Id,
           sacc_start, sacc_end) %>%
  summarise(
    x_start = dplyr::first(x, order_by = time),
    y_start = dplyr::first(y, order_by = time),
    x_end   = dplyr::last(x, order_by = time),
    y_end   = dplyr::last(y, order_by = time),
    .groups = "drop"
  ) %>%
  mutate(
    amplitude_px = sqrt((x_end - x_start)^2 + (y_end - y_start)^2),
    amplitude_deg = amplitude_px * mean(c(0.01865554, 0.01919689)),
    duration_ms = sacc_end - sacc_start
  ) %>%
  filter(amplitude_deg > 0, amplitude_deg < 10)

# Panel A: Saccade amplitude density by algorithm
fig8a <- sac_amplitude %>%
  filter(amplitude_deg < 5) %>%
  ggplot(aes(x = amplitude_deg, fill = method)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ method, ncol = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(title = "A. Detected saccade amplitude distributions",
       x = "Saccade amplitude (deg)", y = "Density") +
  theme_paper

# Panel B: Count vs amplitude (binned)
sac_amp_binned <- sac_amplitude %>%
  mutate(amp_bin = cut(amplitude_deg,
                       breaks = seq(0, 10, by = 0.25),
                       labels = FALSE)) %>%
  filter(!is.na(amp_bin)) %>%
  group_by(method, amp_bin) %>%
  summarise(
    mean_amplitude = mean(amplitude_deg, na.rm = TRUE),
    n_events = n(),
    mean_duration = mean(duration_ms, na.rm = TRUE),
    .groups = "drop"
  )

fig8b <- ggplot(sac_amp_binned,
                aes(x = mean_amplitude, y = n_events, colour = method)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(title = "B. Number of saccades detected vs amplitude",
       x = "Saccade amplitude (deg)", y = "Count") +
  theme_paper

fig8c <- ggplot(sac_amp_binned,
                aes(x = mean_amplitude, y = mean_duration, colour = method)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(title = "C. Mean saccade duration vs amplitude",
       x = "Saccade amplitude (deg)", y = "Mean duration (ms)") +
  theme_paper

fig8 <- fig8a + (fig8b / fig8c) +
  plot_layout(widths = c(1, 1.5))

ggsave(file.path(fig_dir, "Figure8_saccade_amplitude.pdf"),
       fig8, width = 14, height = 8, device = "pdf")
ggsave(file.path(fig_dir, "Figure8_saccade_amplitude.png"),
       fig8, width = 14, height = 8, dpi = 300)
cat("  Figure 8 saved.\n")

# ===========================================================================
# Figure 9: Event-level W_multi bar plot (per method x event)
# ===========================================================================
cat("Generating Figure 9 (W_multi bar)...\n")

fig9_dat <- W_all %>%
  filter(!is.na(W_multi)) %>%
  group_by(method, event) %>%
  slice_max(W_multi, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    method = factor(method, levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM")),
    event  = factor(event, levels = c("fixation", "saccade"),
                    labels = c("Fixation", "Saccade"))
  )

fig9 <- ggplot(fig9_dat, aes(x = method, y = W_multi, fill = event)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", W_multi)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  facet_wrap(~ event, ncol = 2) +
  scale_fill_manual(values = c(Fixation = "#1B9E77", Saccade = "#D95F02"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Event-level Wasserstein similarity (W_multi)",
       subtitle = "Higher values indicate closer agreement with the manual reference",
       x = NULL, y = "W_multi similarity score") +
  theme_paper

ggsave(file.path(fig_dir, "Figure9_Wmulti_bar.pdf"),
       fig9, width = 8, height = 4.5, device = "pdf")
ggsave(file.path(fig_dir, "Figure9_Wmulti_bar.png"),
       fig9, width = 8, height = 4.5, dpi = 300)
cat("  Figure 9 saved.\n")

# ===========================================================================
# Figure 10: Event count + mean duration (needs event_level_stats.csv)
#   -> run manuscript/event_level_stats.R first (GT = full webdata_manual_labels)
# ===========================================================================
cat("Generating Figure 10 (event count/duration)...\n")

ev_stats_file <- file.path(res_dir, "event_level_stats.csv")
if (file.exists(ev_stats_file)) {
  ev_stats <- read_csv(ev_stats_file, show_col_types = FALSE) %>%
    mutate(method = recode(method, "CNN-BLSTM (pred)" = "CNN-BLSTM"),
           event  = recode(event, fixation = "Fixation", saccade = "Saccade"))

  gt_line <- ev_stats %>% filter(method == "Ground truth")
  plot_dat <- ev_stats %>%
    filter(method != "Ground truth") %>%
    mutate(method = factor(method,
                           levels = c("IVT", "IDT", "EK03", "IHMM", "CNN-BLSTM")))

  p10a <- plot_dat %>% filter(event == "Fixation") %>%
    ggplot(aes(x = method, y = n_events)) +
    geom_col(fill = "#1B9E77", width = 0.6) +
    geom_hline(yintercept = gt_line$n_events[gt_line$event == "Fixation"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = n_events), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "A. Fixation event count", x = NULL, y = "Event count") +
    theme_paper

  p10b <- plot_dat %>% filter(event == "Fixation") %>%
    ggplot(aes(x = method, y = mean_dur_ms)) +
    geom_col(fill = "#1B9E77", width = 0.6) +
    geom_hline(yintercept = gt_line$mean_dur_ms[gt_line$event == "Fixation"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = round(mean_dur_ms, 0)), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "B. Fixation mean duration", x = NULL, y = "Mean duration (ms)") +
    theme_paper

  p10c <- plot_dat %>% filter(event == "Saccade") %>%
    ggplot(aes(x = method, y = n_events)) +
    geom_col(fill = "#D95F02", width = 0.6) +
    geom_hline(yintercept = gt_line$n_events[gt_line$event == "Saccade"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = n_events), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "D. Saccade event count", x = NULL, y = "Event count") +
    theme_paper

  p10d <- plot_dat %>% filter(event == "Saccade") %>%
    ggplot(aes(x = method, y = mean_dur_ms)) +
    geom_col(fill = "#D95F02", width = 0.6) +
    geom_hline(yintercept = gt_line$mean_dur_ms[gt_line$event == "Saccade"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = round(mean_dur_ms, 0)), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "E. Saccade mean duration", x = NULL, y = "Mean duration (ms)") +
    theme_paper

  p10e <- plot_dat %>% filter(event == "Fixation") %>%
    ggplot(aes(x = method, y = sd_dur_ms)) +
    geom_col(fill = "#1B9E77", width = 0.6, alpha = 0.7) +
    geom_hline(yintercept = gt_line$sd_dur_ms[gt_line$event == "Fixation"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = round(sd_dur_ms, 0)), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "C. Fixation SD duration", x = NULL, y = "SD duration (ms)") +
    theme_paper

  p10f <- plot_dat %>% filter(event == "Saccade") %>%
    ggplot(aes(x = method, y = sd_dur_ms)) +
    geom_col(fill = "#D95F02", width = 0.6, alpha = 0.7) +
    geom_hline(yintercept = gt_line$sd_dur_ms[gt_line$event == "Saccade"],
               linetype = "dashed", colour = "grey30") +
    geom_text(aes(label = round(sd_dur_ms, 0)), vjust = -0.3, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "F. Saccade SD duration", x = NULL, y = "SD duration (ms)") +
    theme_paper

  fig10 <- (p10a | p10b | p10e) / (p10c | p10d | p10f)
  ggsave(file.path(fig_dir, "Figure10_event_count_duration.pdf"),
         fig10, width = 13, height = 7, device = "pdf")
  ggsave(file.path(fig_dir, "Figure10_event_count_duration.png"),
         fig10, width = 13, height = 7, dpi = 300)
  cat("  Figure 10 saved.\n")
} else {
  cat("  event_level_stats.csv not found - run event_level_stats.R first. Skipping Figure 10.\n")
}

# ===========================================================================
# Table S1: Best parameters summary
# ===========================================================================
cat("\nGenerating Table S1...\n")

table_s1 <- best_all %>%
  mutate(smoothing = ifelse(method == "CNN-BLSTM" & smoothing == "sg",
                            "sg_p5_n23", smoothing)) %>%
  select(method, best_for_event, smoothing, parameter_value, MCC) %>%
  arrange(method, best_for_event) %>%
  rename(
    Method           = method,
    `Best for`       = best_for_event,
    `Best smoothing` = smoothing,
    `Best parameter` = parameter_value,
    MCC              = MCC
  )

write_csv(table_s1, file.path(fig_dir, "TableS1_best_parameters.csv"))
cat("  Table S1 saved.\n")

# ===========================================================================
# Supplementary Figure S1: Full parameter tuning by smoothing (no IHMM)
# ===========================================================================
cat("Generating Supplementary Figure S1...\n")

sup_s1_data <- mcc_all %>%
  filter(method != "IHMM") %>%
  mutate(
    smoothing = factor(smoothing,
                       levels = c("raw", "mean", "median", "sg")),
    method = factor(method, levels = c("IVT", "IDT", "EK03"))
  )

for (m in levels(sup_s1_data$method)) {
  for (ev in c("fixation", "saccade")) {
    p <- sup_s1_data %>%
      filter(method == m, event == ev) %>%
      ggplot(aes(x = parameter_value, y = MCC,
                 colour = smoothing, shape = smoothing)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 2) +
      scale_colour_brewer(palette = "Set1", name = "Smoothing") +
      scale_shape_manual(values = c(16, 17, 15, 18), name = "Smoothing") +
      labs(title = sprintf("%s \u2013 %s MCC", m, tools::toTitleCase(ev)),
           x = "Parameter value", y = "MCC") +
      theme_paper

    fname <- sprintf("FigureS1_%s_%s", m, ev)
    ggsave(file.path(fig_dir, paste0(fname, ".pdf")),
           p, width = 8, height = 5, device = "pdf")
    ggsave(file.path(fig_dir, paste0(fname, ".png")),
           p, width = 8, height = 5, dpi = 300)
  }
}

cat("  Supplementary Figure S1 saved.\n")

