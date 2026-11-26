part2_inputs <- function(root) {
  config <- jsonlite::fromJSON(file.path(root, 'config/part2/protocol.json'))
  manifest_path <- file.path(root, 'config/part2/input_manifest.csv')
  stopifnot(file_hash(manifest_path) == config$input_manifest_sha256)
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, ]
    stopifnot(file_hash(row$path) == row$sha256)
    if (nzchar(row$archive_sha256)) stopifnot(row$archive_sha256 == row$sha256)
  }
  list(config = config, paths = setNames(manifest$path, manifest$id))
}

part2_read <- function(paths, id) {
  connection <- if (grepl('[.]gz$', paths[[id]])) gzfile(paths[[id]], open = 'rt') else file(paths[[id]], open = 'rt')
  on.exit(close(connection))
  read.csv(connection, stringsAsFactors = FALSE, check.names = FALSE)
}

part2_context <- function(paths) {
  trials <- part2_read(paths, 'trials')
  mask <- part2_read(paths, 'mask')
  mask <- mask[mask$common_eligible %in% c(TRUE, 'True'), ]
  key <- function(sub, trial) paste(sub, trial, sep = '/')
  trial_keys <- key(trials$sub, trials$Trial_Id)
  stopifnot(length(trial_keys) == 792, !anyDuplicated(trial_keys), nrow(mask) == 68577, !anyDuplicated(mask$source_row_id))
  index <- match(key(mask$sub, mask$Trial_Id), trial_keys)
  stopifnot(!anyNA(index))
  weight <- tabulate(index, nbins = nrow(trials))
  positive <- lapply(c('fixation', 'saccade'), function(event) tabulate(index[mask$ground_truth == event], nbins = nrow(trials)))
  names(positive) <- c('fixation', 'saccade')
  roster <- list(trials = trials, mask = mask, index = index, keys = trial_keys, weight = weight,
                 positive = positive, participants = sort(unique(trials$sub)))
  defs <- part2_read(paths, 'b_grid')
  hd <- part2_read(paths, 'h_grid')
  hd$threshold_value <- NA_real_
  hd$preprocessing <- c(raw = 'Raw', mean = 'Mean3', median = 'Median3', sg = 'SG5_23')[hd$smoothing]
  columns <- c('configuration_id', 'method', 'event', 'preprocessing', 'smoothing', 'parameter', 'threshold_value')
  roster$definitions <- rbind(defs[columns], hd[columns])
  bc <- part2_read(paths, 'b_counts')
  hc <- part2_read(paths, 'h_counts')
  roster$calls <- list()
  audit <- list()
  for (condition in c('original', 'rate30')) {
    b <- bc[bc$condition == condition, ]
    h <- hc[hc$condition == condition, ]
    all <- rbind(b[c('configuration_id', 'sub', 'Trial_Id', 'status', 'common_rows', 'TP', 'TN', 'FP', 'FN')],
                 h[c('configuration_id', 'sub', 'Trial_Id', 'status', 'common_rows', 'TP', 'TN', 'FP', 'FN')])
    calls <- split(all, all$configuration_id)
    stopifnot(length(calls) == nrow(roster$definitions))
    for (id in names(calls)) {
      call <- calls[[id]]
      stopifnot(nrow(call) == 792, !anyDuplicated(key(call$sub, call$Trial_Id)))
      call <- call[match(trial_keys, key(call$sub, call$Trial_Id)), ]
      ok <- call$status %in% c('success', 'success_empty')
      event <- roster$definitions$event[match(id, roster$definitions$configuration_id)]
      stopifnot(identical(as.integer(call$common_rows), weight),
                all(rowSums(call[ok, c('TP', 'TN', 'FP', 'FN')]) == weight[ok]),
                all(call$TP[ok] + call$FN[ok] == positive[[event]][ok]))
      count <- as.matrix(call[c('TP', 'TN', 'FP', 'FN')])
      stopifnot(all(is.finite(count[ok, , drop = FALSE])))
      count[!ok, ] <- 0
      calls[[id]] <- list(ok = ok, counts = count, status = call$status)
    }
    roster$calls[[condition]] <- calls
    audit[[condition]] <- data.frame(condition = condition, configurations = length(calls),
                                    accounted_calls = nrow(all), trial_rows_aligned = TRUE)
  }
  roster$audit <- do.call(rbind, audit)
  roster
}
