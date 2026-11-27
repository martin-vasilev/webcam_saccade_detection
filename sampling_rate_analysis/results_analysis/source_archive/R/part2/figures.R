part2_figures <- function(pooled, cnn, destination) {
  own <- pooled[pooled$selection_policy == 'shared_preprocessing_joint' & pooled$evaluation_support == 'own_policy_paired_joint', ]
  shared <- pooled[pooled$evaluation_support == 'shared_policy_paired_joint', ]
  sources <- list(joint = own, cnn = cnn, policies = shared)
  colours <- c(original = '#3862a4', rate30 = '#c45f31')
  method_labels <- c(IVT = 'I-VT', IDT = 'I-DT', EK03 = 'EK03', IHMM = 'I-HMM')
  ledger <- list(); captions <- list()
  record_points <- function(data, i, x, metric, colour, record, figure, table) {
    points(x, data[[metric]][i], col = colour, pch = 16, cex = .9)
    if (record) ledger[[length(ledger) + 1]] <<- data.frame(figure = figure, table = table, source_row = i,
       metric = metric, x = x, value = data[[metric]][i], evaluation_rows = data$evaluation_rows[i])
  }
  save <- function(name, width, height, draw, caption) {
    for (extension in c('pdf', 'png')) {
      path <- file.path(destination, 'figures', paste0(name, '.', extension))
      if (extension == 'pdf') pdf(path, width = width, height = height, useDingbats = FALSE)
      else png(path, width = width, height = height, units = 'in', res = 300, type = 'cairo')
      draw(extension == 'pdf')
      mtext(paste(strwrap(caption, width = 145), collapse = '\n'), outer = TRUE, side = 1, line = 1, adj = 0, cex = .65)
      dev.off()
    }
    captions[[name]] <<- caption
  }
  for (metric in c('MCC', 'F1')) {
    name <- paste0('joint_policy_', metric)
    save(name, 11, 6.2, function(record) {
      par(mfrow = c(2, 4), mar = c(3, 5, 3, 1), oma = c(5.5, .5, 2, .5), cex.axis = .8, cex.lab = .85)
      for (event in c('fixation', 'saccade')) for (method in c('IVT', 'IDT', 'EK03', 'IHMM')) {
        i <- which(own$algorithm == method & own$event == event)
        i <- i[match(c('original', 'rate30'), own$representation[i])]
        plot(NA, xlim = c(.7, 2.3), ylim = c(if (metric == 'MCC') -.15 else 0, 1), xaxt = 'n', xlab = '', ylab = metric, bty = 'l')
        axis(1, 1:2, c('Original', '30 Hz'), cex.axis = .75)
        mtext(paste(method_labels[method], event), 3, 1.5, cex = .8, font = 2)
        mtext(paste0('n=', own$evaluation_rows[i[1]]), 3, .2, cex = .65)
        record_points(own, i, 1:2, metric, colours[own$representation[i]], record, name, 'joint')
      }
    }, 'Training-only joint-MCC selection with shared preprocessing and event-specific thresholds. Both tasks/rates use the same own-policy paired joint support within algorithm. Algorithm supports differ. HMM primary seed 0.')
  }
  save('cnn_five_branches_MCC_F1', 11, 7, function(record) {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(5, .5, 2, .5))
    branches <- c('raw', 'mean', 'median', 'sg_p3_n7', 'sg_p5_n23')
    for (metric in c('MCC', 'F1')) for (event in c('fixation', 'saccade')) {
      plot(NA, xlim = c(.6, 5.4), ylim = c(0, 1), xaxt = 'n', xlab = '', ylab = metric, bty = 'l')
      axis(1, 1:5, c('Raw', 'Mean3', 'Median3', 'SG3/7', 'SG5/23'), cex.axis = .8)
      mtext(paste('CNN-BLSTM', event, metric), 3, 1.2, font = 2, cex = .9)
      for (j in seq_along(colours)) {
        i <- which(cnn$representation == names(colours)[j] & cnn$event == event)
        i <- i[match(branches, cnn$branch[i])]
        record_points(cnn, i, (1:5) + (j - 1.5) * .15, metric, colours[j], record, 'cnn_five_branches_MCC_F1', 'cnn')
      }
      legend('bottomleft', c('Original', '30 Hz'), pch = 16, col = colours, bty = 'n', cex = .75)
    }
  }, 'All five fixed held-out CNN branches; no branch selection. Every branch/rate/event uses 68,577 frozen reference rows. Original and 30-Hz Option A training budgets differ.')
  save('policy_comparison_matched_support', 12, 6.5, function(record) {
    par(mfrow = c(2, 4), mar = c(4, 5, 3, 1), oma = c(5, .5, 2, .5), cex.axis = .8, cex.lab = .85)
    policies <- c('historical_fixed', 'existing_event_specific', 'shared_preprocessing_joint')
    for (event in c('fixation', 'saccade')) for (method in c('IVT', 'IDT', 'EK03', 'IHMM')) {
      plot(NA, xlim = c(.6, 3.4), ylim = c(-.15, .6), xaxt = 'n', xlab = '', ylab = 'Pooled MCC', bty = 'l')
      axis(1, 1:3, c('Fixed', 'Event-specific', 'Joint'), cex.axis = .65)
      mtext(paste(method_labels[method], event), 3, 1.5, cex = .8, font = 2)
      idx <- which(shared$algorithm == method & shared$event == event)
      mtext(paste0('shared n=', shared$evaluation_rows[idx[1]]), 3, .2, cex = .65)
      for (j in seq_along(colours)) {
        i <- idx[shared$representation[idx] == names(colours)[j]]
        i <- i[match(policies, shared$selection_policy[i])]
        record_points(shared, i, (1:3) + (j - 1.5) * .12, 'MCC', colours[j], record, 'policy_comparison_matched_support', 'policies')
      }
    }
  }, 'Identical success rows across all three policies, both rates and events within algorithm. Blue=Original; orange=30 Hz. Frozen original-policy supports are retained separately. These descriptive comparisons do not select a winning policy.')
  write.csv(do.call(rbind, ledger), file.path(destination, 'validation/plot_ledger.csv'), row.names = FALSE, na = '')
  for (name in names(sources)) write.csv(sources[[name]], file.path(destination, 'validation', paste0('plot_source_', name, '.csv')), row.names = FALSE, na = '')
  jsonlite::write_json(captions, file.path(destination, 'figures/captions.json'), auto_unbox = TRUE, pretty = TRUE)
}
