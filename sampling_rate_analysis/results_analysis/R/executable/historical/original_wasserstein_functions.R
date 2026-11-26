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
    W_n     = w_dist(gt_mat, algo_mat, 1),   # event count
    W_mean  = w_dist(gt_mat, algo_mat, 2),   # mean duration
    W_sd    = w_dist(gt_mat, algo_mat, 3)    # SD duration
  )
}

#Traditional algorithms: per (method, smoothing, parameter) 
run_wasserstein_trad <- function(comp_df, event_type) {
  if (event_type == "fixation") {
    n_c  <- "algo_n_fixations";  m_c  <- "algo_mean_fix_dur";  s_c  <- "algo_sd_fix_dur"
    gn_c <- "GT_n_fixations";    gm_c <- "GT_mean_fix_dur";    gs_c <- "GT_sd_fix_dur"
  } else {
    n_c  <- "algo_n_saccades";   m_c  <- "algo_mean_sacc_dur"; s_c  <- "algo_sd_sacc_dur"
    gn_c <- "GT_n_saccades";     gm_c <- "GT_mean_sacc_dur";   gs_c <- "GT_sd_sacc_dur"
  }

  comp_df %>%
    group_by(method, smoothing, parameter) %>%
    group_modify(~ {
      gt   <- .x %>% select(all_of(c(gn_c, gm_c, gs_c))) %>% as.matrix()
      algo <- .x %>% select(all_of(c(n_c,  m_c,  s_c)))  %>% as.matrix()
      w <- compute_wasserstein_full(gt, algo)
      data.frame(W_multi = w$W_multi, W_n = w$W_n, W_mean = w$W_mean, W_sd = w$W_sd)
    }) %>%
    ungroup() %>%
    mutate(event = event_type)
}
