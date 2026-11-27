classification_metrics <- function(counts) {
  counts <- unname(counts)
  tp <- counts[1]; tn <- counts[2]; fp <- counts[3]; fn <- counts[4]
  ratio <- function(a, b) if (b == 0) NA_real_ else a / b
  c(MCC = joint_mcc(counts), F1 = ratio(2 * tp, 2 * tp + fp + fn),
    precision = ratio(tp, tp + fp), recall = ratio(tp, tp + fn))
}

policy_calls <- function(context, selected, paths) {
  old <- rbind(part2_read(paths, 'b_old')[c('heldout_sub', 'condition', 'method', 'event', 'configuration_id')],
               part2_read(paths, 'h_old')[c('heldout_sub', 'condition', 'method', 'event', 'configuration_id')])
  old <- unique(old[old$condition %in% c('original', 'rate30'), ])
  fixed <- part2_read(paths, 'fixed')
  rows <- list()
  for (heldout in context$participants) for (condition in c('original', 'rate30')) for (method in c('IVT', 'IDT', 'EK03', 'IHMM')) {
    joint <- selected[selected$heldout_sub == heldout & selected$condition == condition & selected$method == method, ]
    stopifnot(nrow(joint) == 1, joint$outcome == 'selected')
    for (event in c('fixation', 'saccade')) {
      prior <- old[old$heldout_sub == heldout & old$condition == condition & old$method == method & old$event == event, ]
      stopifnot(nrow(prior) == 1)
      if (method == 'IHMM') fixed_id <- paste0('IHMM__', event, '__sg__none') else {
        f <- fixed[fixed$method == method & fixed$event == event, ]
        defs <- context$definitions
        match <- defs[defs$method == method & defs$event == event & defs$smoothing == f$smoothing & defs$parameter == f$parameter, ]
        stopifnot(nrow(match) == 1)
        fixed_id <- match$configuration_id
      }
      ids <- c(historical_fixed = fixed_id, existing_event_specific = prior$configuration_id,
               shared_preprocessing_joint = if (event == 'fixation') joint$fix_id else joint$sacc_id)
      for (policy in names(ids)) {
        def <- context$definitions[context$definitions$configuration_id == ids[[policy]], ]
        rows[[length(rows) + 1]] <- data.frame(heldout_sub = heldout, condition = condition, method = method,
          event = event, selection_policy = policy, configuration_id = ids[[policy]],
          preprocessing = def$preprocessing, threshold = def$threshold_value)
      }
    }
  }
  do.call(rbind, rows)
}

evaluate_policies <- function(context, choices) {
  trial_results <- list(); summaries <- list(); participants <- list()
  for (method in c('IVT', 'IDT', 'EK03', 'IHMM')) {
    method_choices <- choices[choices$method == method, ]
    callset <- list(); paired <- list()
    for (policy in unique(method_choices$selection_policy)) {
      for (condition in c('original', 'rate30')) for (event in c('fixation', 'saccade')) {
        counts <- matrix(0, nrow = nrow(context$trials), ncol = 4)
        ok <- logical(nrow(context$trials)); status <- rep('', length(ok)); cfg <- status
        q <- method_choices[method_choices$selection_policy == policy & method_choices$condition == condition & method_choices$event == event, ]
        for (row in seq_len(nrow(q))) {
          keep <- context$trials$sub == q$heldout_sub[row]
          call <- context$calls[[condition]][[q$configuration_id[row]]]
          counts[keep, ] <- call$counts[keep, ]; ok[keep] <- call$ok[keep]
          status[keep] <- call$status[keep]; cfg[keep] <- q$configuration_id[row]
        }
        callset[[paste(policy, condition, event)]] <- list(counts = counts, ok = ok, status = status, configuration = cfg)
      }
      paired[[policy]] <- Reduce('&', lapply(c('original', 'rate30'), function(condition) {
        callset[[paste(policy, condition, 'fixation')]]$ok & callset[[paste(policy, condition, 'saccade')]]$ok
      }))
    }
    shared <- Reduce('&', paired)
    for (policy in names(paired)) for (condition in c('original', 'rate30')) {
      joint <- callset[[paste(policy, condition, 'fixation')]]$ok & callset[[paste(policy, condition, 'saccade')]]$ok
      for (event in c('fixation', 'saccade')) {
        call <- callset[[paste(policy, condition, event)]]
        trial_results[[length(trial_results) + 1]] <- data.frame(context$trials, condition = condition, method = method,
          event = event, selection_policy = policy, configuration_id = call$configuration, status = call$status,
          call_available = call$ok, condition_joint_available = joint, own_policy_paired_joint = paired[[policy]],
          shared_policy_paired_joint = shared, common_rows = context$weight,
          TP = call$counts[, 1], TN = call$counts[, 2], FP = call$counts[, 3], FN = call$counts[, 4])
        masks <- list(condition_specific_joint = joint, own_policy_paired_joint = paired[[policy]], shared_policy_paired_joint = shared)
        for (support in names(masks)) {
          keep <- masks[[support]]
          count <- colSums(call$counts[keep, , drop = FALSE])
          id <- joint_hash(sort(context$mask$source_row_id[keep[context$index]]))
          summaries[[length(summaries) + 1]] <- data.frame(algorithm = method, representation = condition, event = event,
            preprocessing = 'fold-specific; see choices', threshold = 'fold-specific; see choices',
            selection_policy = policy, evaluation_support = support, heldout_participants = length(context$participants),
            evaluation_rows = sum(context$weight[keep]), available_trials = sum(joint), failed_trials = sum(!joint),
            successful_event_calls = sum(call$ok), failed_event_calls = sum(!call$ok), scored_trials = sum(keep),
            scorable_trials = sum(keep & context$weight > 0), row_id_sha256 = id,
            TP = count[1], TN = count[2], FP = count[3], FN = count[4], as.list(classification_metrics(count)))
          for (sub in context$participants) {
            k <- keep & context$trials$sub == sub
            co <- colSums(call$counts[k, , drop = FALSE])
            participants[[length(participants) + 1]] <- data.frame(algorithm = method, representation = condition,
              event = event, selection_policy = policy, evaluation_support = support, sub = sub,
              evaluation_rows = sum(context$weight[k]), TP = co[1], TN = co[2], FP = co[3], FN = co[4],
              as.list(classification_metrics(co)))
          }
        }
      }
    }
  }
  pooled <- do.call(rbind, summaries)
  keys <- paste(pooled$algorithm, pooled$representation, pooled$selection_policy, pooled$evaluation_support)
  pooled$joint_descriptive_mean_MCC <- vapply(seq_len(nrow(pooled)), function(i) {
    mean(pooled$MCC[keys == keys[i]])
  }, numeric(1))
  list(trials = do.call(rbind, trial_results), pooled = pooled, participants = do.call(rbind, participants))
}

evaluate_cnn <- function(paths) {
  counts <- part2_read(paths, 'cnn_counts')
  counts <- counts[counts$scope == 'cnn_primary', ]
  rows <- list()
  for (algorithm in unique(counts$algorithm)) for (condition in c('original', 'rate30')) for (event in c('fixation', 'saccade')) {
    q <- counts[counts$algorithm == algorithm & counts$condition == condition & counts$event == event, ]
    stopifnot(nrow(q) == 20, !anyDuplicated(q$sub))
    co <- colSums(q[c('TP', 'TN', 'FP', 'FN')])
    availability <- part2_read(paths, paste0('cnn_', condition, '_availability'))
    av <- availability[availability$branch == sub('^CNN_', '', algorithm), ]
    rows[[length(rows) + 1]] <- data.frame(algorithm = 'CNN-BLSTM', branch = sub('^CNN_', '', algorithm),
      representation = condition, event = event, preprocessing = sub('^CNN_', '', algorithm), threshold = 'not applicable',
      selection_policy = 'fixed held-out branch; no branch selector', heldout_participants = nrow(q),
      evaluation_support = 'frozen_CNN_common', evaluation_rows = sum(q$n),
      available_trials = av$successful_trials, failed_trials = av$failed_trials,
      TP = co[1], TN = co[2], FP = co[3], FN = co[4], as.list(classification_metrics(co)))
  }
  result <- do.call(rbind, rows)
  key <- paste(result$branch, result$representation)
  result$joint_descriptive_mean_MCC <- vapply(seq_len(nrow(result)), function(i) mean(result$MCC[key == key[i]]), numeric(1))
  result
}
