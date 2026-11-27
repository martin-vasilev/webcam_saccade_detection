load_csv <- function(paths, key) read.csv(paths[[key]], stringsAsFactors = FALSE, check.names = FALSE)

import_part1 <- function(paths, config) {
  filter_primary <- function(d) d[d$condition %in% config$conditions & d$policy == 'primary' & d$estimand == 'paired_cross_rate', , drop = FALSE]
  grid <- filter_primary(load_csv(paths, 'b1'))
  hmm <- filter_primary(load_csv(paths, 'ihmm'))
  hmm$preprocessing <- c(raw = 'Raw', mean = 'Mean3', median = 'Median3', sg = 'SG5_23')[hmm$smoothing]
  cnn <- load_csv(paths, 'cnn')
  cnn <- cnn[cnn$scope == 'cnn_primary', ]
  selected <- filter_primary(load_csv(paths, 'b2'))
  hmm_selected <- filter_primary(load_csv(paths, 'ihmm_selected'))
  stopifnot(nrow(grid) == 640, nrow(hmm) == 16, nrow(cnn) == 10, nrow(selected) == 12, nrow(hmm_selected) == 4)
  stopifnot(!anyDuplicated(grid[c('configuration_id', 'condition')]), all(cnn$n == 68577), all(cnn$participants == 20))
  make_common <- function(d, method) {
    subset <- d[d$method == method & d$preprocessing == 'SG5_23', ]
    keep <- vapply(seq_len(nrow(subset)), function(i) {
      threshold <- config$common_sg$thresholds[[method]][[subset$event[i]]]
      subset$threshold[i] == threshold
    }, logical(1))
    subset <- subset[keep, ]
    data.frame(method = method, event = subset$event, condition = subset$condition,
               preprocessing = 'SG5_23', threshold = subset$threshold, MCC = subset$MCC,
               scored_rows = subset$scored_rows, source_id = 'b1', configuration_id = subset$configuration_id)
  }
  common <- do.call(rbind, lapply(c('IVT', 'IDT', 'EK03'), function(m) make_common(grid, m)))
  hh <- hmm[hmm$smoothing == 'sg', ]
  common <- rbind(common, data.frame(method = 'IHMM', event = hh$event, condition = hh$condition,
                                    preprocessing = 'SG5_23', threshold = NA_real_, MCC = hh$MCC,
                                    scored_rows = hh$scored_rows, source_id = 'ihmm', configuration_id = hh$configuration_id))
  branch_names <- c(CNN_raw = 'Raw', CNN_mean = 'Mean3', CNN_median = 'Median3', CNN_sg_p3_n7 = 'SG3_7', CNN_sg_p5_n23 = 'SG5_23')
  cn <- do.call(rbind, lapply(config$conditions, function(condition) {
    data.frame(method = 'CNN', event = cnn$event, condition = condition, preprocessing = unname(branch_names[cnn$algorithm]),
               MCC = if (condition == 'original') cnn$Original_MCC else cnn$MCC_30Hz,
               scored_rows = cnn$n, algorithm = cnn$algorithm, source_id = 'cnn')
  }))
  cc <- cn[cn$preprocessing == 'SG5_23', ]
  common <- rbind(common, data.frame(method = 'CNN', event = cc$event, condition = cc$condition,
                                    preprocessing = cc$preprocessing, threshold = NA_real_, MCC = cc$MCC,
                                    scored_rows = cc$scored_rows, source_id = 'cnn', configuration_id = 'CNN_sg_p5_n23'))
  stopifnot(nrow(common) == 20, all(is.finite(common$MCC)))
  list(grid = grid, hmm = hmm, cnn = cn, cnn_primary = cnn, selected = selected,
       hmm_selected = hmm_selected, common = common, grid_definitions = load_csv(paths, 'grid'))
}

write_tables <- function(d, destination) {
  for (name in c('grid', 'hmm', 'cnn', 'cnn_primary', 'selected', 'hmm_selected', 'common', 'grid_definitions')) {
    write.csv(d[[name]], file.path(destination, 'tables', paste0(name, '_source.csv')), row.names = FALSE, na = '')
  }
  compact <- list()
  for (name in c('grid', 'hmm', 'cnn', 'selected', 'hmm_selected', 'common')) {
    x <- d[[name]]
    for (key in unique(paste(x$method, x$event, x$condition))) {
      g <- x[paste(x$method, x$event, x$condition) == key, ]
      compact[[length(compact) + 1]] <- data.frame(product = name, method = g$method[1], event = g$event[1],
        condition = g$condition[1], configurations = nrow(g), minimum_scored_rows = min(g$scored_rows),
        maximum_scored_rows = max(g$scored_rows), undefined_MCC = sum(!is.finite(g$MCC)))
    }
  }
  write.csv(do.call(rbind, compact), file.path(destination, 'tables/configurations_and_support.csv'), row.names = FALSE)
}
