
# Extract ground-truth fixation events

gt_fixation_events <- dat %>%
  dplyr::transmute(
    sub = sub,
    Trial_Id = Trial_Id,
    time = time_start,
    GT = ground_truth
  ) %>%
  dplyr::arrange(sub, Trial_Id, time) %>%
  dplyr::group_by(sub, Trial_Id) %>%
  dplyr::mutate(
    is_fix = GT == "fixation",
    event_id = cumsum(is_fix != dplyr::lag(is_fix, default = first(is_fix)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is_fix) %>%
  dplyr::group_by(sub, Trial_Id, event_id) %>%
  dplyr::summarise(
    fix_start = min(time, na.rm = TRUE),
    fix_end = max(time, na.rm = TRUE),
    duration = fix_end - fix_start,
    .groups = "drop"
  )

gt_fixation_summary <- gt_fixation_events %>%
  dplyr::group_by(sub, Trial_Id) %>%
  dplyr::summarise(
    GT_n_fixations = dplyr::n(),
    GT_mean_fix_dur = mean(duration, na.rm = TRUE),
    GT_sd_fix_dur = sd(duration, na.rm = TRUE),
    .groups = "drop"
  )
# Extract ground-truth saccade events

gt_saccade_events <- dat %>%
  dplyr::transmute(
    sub = sub,
    Trial_Id = Trial_Id,
    time = time_start,
    GT = ground_truth
  ) %>%
  dplyr::arrange(sub, Trial_Id, time) %>%
  dplyr::group_by(sub, Trial_Id) %>%
  dplyr::mutate(
    is_sacc = GT == "saccade",
    event_id = cumsum(is_sacc != dplyr::lag(is_sacc, default = first(is_sacc)))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is_sacc) %>%
  dplyr::group_by(sub, Trial_Id, event_id) %>%
  dplyr::summarise(
    sacc_start = min(time, na.rm = TRUE),
    sacc_end = max(time, na.rm = TRUE),
    duration = sacc_end - sacc_start,
    .groups = "drop"
  )

gt_saccade_summary <- gt_saccade_events %>%
  dplyr::group_by(sub, Trial_Id) %>%
  dplyr::summarise(
    GT_n_saccades = dplyr::n(),
    GT_mean_sacc_dur = mean(duration, na.rm = TRUE),
    GT_sd_sacc_dur = sd(duration, na.rm = TRUE),
    .groups = "drop"
  )
