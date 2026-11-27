joint_mcc <- function(x) {
  denominator <- prod(c(x[1] + x[3], x[1] + x[4], x[2] + x[3], x[2] + x[4]))
  if (denominator == 0) return(NA_real_)
  (x[1] * x[2] - x[3] * x[4]) / sqrt(denominator)
}

joint_hash <- function(indices) digest::digest(paste(indices, collapse = ','), algo = 'sha256', serialize = FALSE)

joint_pairs <- function(definitions, method) {
  rows <- list()
  for (pre in c('Raw', 'Mean3', 'Median3', 'SG5_23')) {
    d <- definitions[definitions$method == method & definitions$preprocessing == pre, ]
    fix <- d[d$event == 'fixation', ]; sac <- d[d$event == 'saccade', ]
    fix <- fix[order(fix$threshold_value, na.last = TRUE), ]
    sac <- sac[order(sac$threshold_value, na.last = TRUE), ]
    for (i in seq_len(nrow(fix))) for (j in seq_len(nrow(sac))) {
      rows[[length(rows) + 1]] <- data.frame(preprocessing = pre, fix_id = fix$configuration_id[i],
         sacc_id = sac$configuration_id[j], fix_threshold = fix$threshold_value[i], sacc_threshold = sac$threshold_value[j])
    }
  }
  do.call(rbind, rows)
}

select_joint <- function(context) {
  selections <- list(); candidates <- list(); training <- list()
  for (condition in c('original', 'rate30')) for (method in c('IVT', 'IDT', 'EK03', 'IHMM')) {
    pairs <- joint_pairs(context$definitions, method)
    calls <- context$calls[[condition]]
    availability <- sapply(seq_len(nrow(pairs)), function(i) calls[[pairs$fix_id[i]]]$ok & calls[[pairs$sacc_id[i]]]$ok)
    for (heldout in context$participants) {
      train <- context$trials$sub != heldout
      training_weights <- context$weight * train
      available_rows <- colSums(availability * training_weights)
      maximum <- max(available_rows)
      tied <- which(available_rows == maximum)
      aligned <- train & apply(availability[, tied, drop = FALSE], 1, all)
      fixed_rows <- sum(context$weight[aligned])
      scores <- rep(NA_real_, nrow(pairs)); fm <- scores; sm <- scores
      for (i in tied) {
        fm[i] <- joint_mcc(colSums(calls[[pairs$fix_id[i]]]$counts[aligned, , drop = FALSE]))
        sm[i] <- joint_mcc(colSums(calls[[pairs$sacc_id[i]]]$counts[aligned, , drop = FALSE]))
        scores[i] <- (fm[i] + sm[i]) / 2
      }
      admissible <- tied[is.finite(scores[tied])]
      winner <- if (maximum > 0 && fixed_rows > 0 && length(admissible)) admissible[which(scores[admissible] >= max(scores[admissible]) - 1e-12)[1]] else NA_integer_
      outcome <- if (is.na(winner)) 'no_admissible_joint_score' else 'selected'
      identity <- joint_hash(sort(context$mask$source_row_id[aligned[context$index]]))
      training[[length(training) + 1]] <- data.frame(heldout_sub = heldout, condition = condition, method = method,
        training_participants = paste(context$participants[context$participants != heldout], collapse = ','),
        aligned_training_trials = paste(which(aligned), collapse = ','), aligned_rows = fixed_rows, row_id_sha256 = identity)
      for (i in seq_len(nrow(pairs))) {
        candidates[[length(candidates) + 1]] <- data.frame(heldout_sub = heldout, condition = condition, method = method,
          candidate_order = i, pairs[i, ], joint_available_rows = available_rows[i], maximum_availability = i %in% tied,
          unavailable_training_trials = paste(which(train & !availability[, i]), collapse = ','),
          aligned_training_rows = fixed_rows, aligned_training_row_sha256 = identity,
          fixation_training_MCC = fm[i], saccade_training_MCC = sm[i], J = scores[i], selected = identical(i, winner))
      }
      chosen <- if (is.na(winner)) data.frame(preprocessing = NA_character_, fix_id = NA_character_, sacc_id = NA_character_, fix_threshold = NA_real_, sacc_threshold = NA_real_) else pairs[winner, ]
      selections[[length(selections) + 1]] <- data.frame(heldout_sub = heldout, condition = condition, method = method,
        chosen, outcome = outcome, maximum_available_training_rows = maximum, aligned_training_rows = fixed_rows,
        training_J = if (is.na(winner)) NA_real_ else scores[winner], aligned_training_row_sha256 = identity)
    }
  }
  list(selections = do.call(rbind, selections), candidates = do.call(rbind, candidates), training = do.call(rbind, training))
}
