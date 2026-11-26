plot_ledger <- list()

plot_series <- function(table, indices, x, figure, panel, source_table, colour, symbol = 16, connect = TRUE) {
  y <- table$MCC[indices]
  plot_ledger[[length(plot_ledger) + 1]] <<- data.frame(figure = figure, panel = panel, source_table = source_table,
    source_row = indices, x = x, MCC = y, scored_rows = table$scored_rows[indices])
  lines(x, y, type = if (connect) 'b' else 'p', col = colour, pch = symbol, lwd = 1.7, cex = .85)
}

row_note <- function(d) {
  sprintf('n = %s to %s', format(min(d$scored_rows), big.mark = ','), format(max(d$scored_rows), big.mark = ','))
}

limits <- function(d) {
  range <- range(c(d$MCC[is.finite(d$MCC)], 0))
  range + c(-.035, .055)
}

panel_axes <- function(x, labels, ylim, title, xlab = '') {
  plot(NA, xlim = range(x) + c(-.3, .3), ylim = ylim, xaxt = 'n', xlab = xlab, ylab = 'Pooled MCC',
       main = '', bty = 'l', cex.main = 1, col.main = '#17364b')
  mtext(title, side = 3, line = 1.55, font = 2, cex = .9, col = '#17364b')
  axis(1, at = x, labels = labels, cex.axis = .8)
  abline(h = 0, col = '#b3c1ca', lwd = .7)
}

save_figure <- function(name, width, height, draw, caption, destination) {
  for (extension in c('pdf', 'png')) {
    file <- file.path(destination, 'figures', paste0(name, '.', extension))
    if (extension == 'pdf') pdf(file, width = width, height = height, family = 'Helvetica', useDingbats = FALSE)
    else png(file, width = width, height = height, units = 'in', res = 300, type = 'cairo')
    # Record actual plotting inputs once; both device runs use the same function/data.
    previous <- length(plot_ledger)
    draw()
    mtext('PART 1  |  Frozen scientific outputs', outer = TRUE, side = 3, line = .7, adj = 0, font = 2, cex = .85, col = '#006a75')
    for (i in seq_along(caption)) mtext(caption[i], outer = TRUE, side = 1, line = 1 + (i - 1) * .95, adj = 0, cex = .7)
    dev.off()
    if (extension == 'png') plot_ledger <<- head(plot_ledger, previous)
  }
}

draw_figures <- function(d, destination) {
  colours <- c(Raw = '#007c83', Mean3 = '#d06b32', Median3 = '#4768b2', SG5_23 = '#83559d')
  conditions <- c('original', 'rate30')
  events <- c('fixation', 'saccade')
  captions <- list()
  for (method in c('IVT', 'IDT', 'EK03')) {
    name <- paste0('01_', method, '_parameter_smoothing')
    caption <- c('Exploratory whole-cohort grid scores; these curves do not select an independently validated optimum.',
                 'Each configuration uses its own successful three-rate paired Original-timestamp support. Support can vary across curves/thresholds.',
                 'Original and 30 Hz are shown. Exact counts and definitions are in grid_source.csv and grid_definitions_source.csv.')
    draw <- function() {
      par(mfrow = c(2, 2), mar = c(4.1, 4.2, 3.1, 1), oma = c(5.6, .5, 2.8, .5), mgp = c(2.5, .8, 0))
      for (event in events) for (condition in conditions) {
        g <- d$grid[d$grid$method == method & d$grid$event == event & d$grid$condition == condition, ]
        x <- sort(unique(g$threshold))
        title <- paste(method, event, if (condition == 'original') '| Original' else '| 30 Hz')
        unit <- switch(method, IVT = 'Velocity threshold (deg/s)', IDT = 'Dispersion threshold (degrees)', EK03 = 'Lambda')
        panel_axes(x, format(x, trim = TRUE), limits(d$grid[d$grid$method == method & d$grid$event == event, ]), title, unit)
        mtext(row_note(g), side = 3, line = .2, cex = .72)
        for (pre in names(colours)) {
          i <- which(d$grid$method == method & d$grid$event == event & d$grid$condition == condition & d$grid$preprocessing == pre)
          i <- i[order(d$grid$threshold[i])]
          plot_series(d$grid, i, d$grid$threshold[i], name, title, 'grid', colours[[pre]])
        }
        legend('topright', legend = c('Raw', 'Mean3', 'Median3', 'SG5/23'), col = colours, lty = 1, pch = 16, bty = 'n', cex = .66, ncol = 2)
      }
    }
    save_figure(name, 10, 7.5, draw, caption, destination)
    captions[[name]] <- caption
  }
  name <- '02_IHMM_preprocessing'
  caption <- c('Primary seed schedule 0. HMM states are re-estimated per trial and representation; this is a preprocessing landscape.',
               'Whole-cohort scores use preprocessing-specific three-rate paired supports. No preprocessing is selected from these plotted scores.',
               'Training-only selected-policy results are shown separately. Exact row identities are retained in hmm_source.csv.')
  draw <- function() {
    par(mfrow = c(1, 2), mar = c(4, 4.2, 3, 1), oma = c(5, .5, 2.8, .5))
    for (event in events) {
      panel_axes(1:4, c('Raw', 'Mean3', 'Median3', 'SG5/23'), limits(d$hmm) + c(0, .10), paste('I-HMM', event))
      mtext(row_note(d$hmm[d$hmm$event == event, ]), side = 3, line = .2, cex = .75)
      for (j in seq_along(conditions)) {
        i <- which(d$hmm$event == event & d$hmm$condition == conditions[j])
        i <- i[match(names(colours), d$hmm$preprocessing[i])]
        plot_series(d$hmm, i, 1:4, name, event, 'hmm', c('#4768b2', '#d06b32')[j], j + 14, FALSE)
      }
      legend('topright', c('Original', '30 Hz'), pch = c(15, 16), col = c('#4768b2', '#d06b32'), bty = 'n', cex = .8)
    }
  }
  save_figure(name, 10, 4.5, draw, caption, destination)
  captions[[name]] <- caption
  name <- '03_CNN_five_preprocessing'
  caption <- c('Corrected CNN-BLSTM; 20 outer participant-held-out models per branch/rate. All five branches are shown without branch selection.',
               'Each point uses the same 68,577 Original reference rows. Original and 30-Hz Option A have different training-window/update budgets.',
               'Point estimates are imported from the validated downstream table. No new intervals or bootstrap draws are calculated.')
  draw <- function() {
    par(mfrow = c(1, 2), mar = c(5, 4.2, 3, 1), oma = c(5, .5, 2.8, .5))
    pre <- c('Raw', 'Mean3', 'Median3', 'SG3_7', 'SG5_23')
    for (event in events) {
      panel_axes(1:5, c('Raw', 'Mean3', 'Median3', 'SG3/7', 'SG5/23'), limits(d$cnn) + c(0, .10), paste('CNN-BLSTM', event))
      mtext('n = 68,577 in both conditions', side = 3, line = .2, cex = .75)
      for (j in seq_along(conditions)) {
        i <- which(d$cnn$event == event & d$cnn$condition == conditions[j])
        i <- i[match(pre, d$cnn$preprocessing[i])]
        plot_series(d$cnn, i, (1:5) + (j - 1.5) * .15, name, event, 'cnn', c('#4768b2', '#d06b32')[j], j + 14, FALSE)
      }
      legend('topright', c('Original', '30 Hz'), pch = c(15, 16), col = c('#4768b2', '#d06b32'), bty = 'n', cex = .8)
    }
  }
  save_figure(name, 10, 4.5, draw, caption, destination)
  captions[[name]] <- caption
  for (product in c('common', 'selected_context')) {
    is_common <- product == 'common'
    if (is_common) x <- d$common else {
      cols <- c('method', 'event', 'condition', 'MCC', 'scored_rows')
      x <- rbind(d$selected[cols], d$hmm_selected[cols])
    }
    table_name <- if (is_common) 'common' else 'selected_context'
    if (!is_common) write.csv(x, file.path(destination, 'tables/selected_context_source.csv'), row.names = FALSE)
    name <- if (is_common) '04_common_SG523' else '05_existing_selected_policy_context'
    methods <- if (is_common) c('IVT', 'IDT', 'EK03', 'IHMM', 'CNN') else c('IVT', 'IDT', 'EK03', 'IHMM')
    caption <- if (is_common) c('SG order 5 / window 23 in every panel. Classical thresholds are historical fixed values, with existing SG grid outputs.',
      'Separate panels have their own successful paired supports. These scores are not a harmonised cross-algorithm inferential comparison.',
      'I-HMM is per-trial re-estimation, primary seed 0; CNN is corrected held-out retraining. Exact settings/supports are in common_source.csv.') else
      c('Existing held-out selected policies only. Training-only availability-first selection is followed by training MCC; no new selector is run.',
        'Classical methods select preprocessing/threshold; I-HMM selects preprocessing among four candidates, primary seed 0.',
        'Panels use method/event-specific three-rate paired support. Selected settings vary by fold/rate; this is context for the exploratory grids.')
    draw <- function() {
      par(mfrow = c(2, length(methods)), mar = c(3.5, 3.8, 3, .7), oma = c(5, .5, 2.8, .5), mgp = c(2.1, .65, 0))
      for (event in events) for (method in methods) {
        i <- which(x$event == event & x$method == method)
        i <- i[match(conditions, x$condition[i])]
        title <- paste(method, event)
        panel_axes(1:2, c('Original', '30 Hz'), limits(x), title)
        mtext(row_note(x[i, ]), side = 3, line = .2, cex = .58)
        plot_series(x, i, 1:2, name, title, table_name, '#007c83')
      }
    }
    save_figure(name, if (is_common) 14 else 12, 7, draw, caption, destination)
    captions[[name]] <- caption
  }
  write.csv(do.call(rbind, plot_ledger), file.path(destination, 'validation/plot_ledger.csv'), row.names = FALSE, na = '')
  jsonlite::write_json(captions, file.path(destination, 'figures/captions.json'), auto_unbox = TRUE, pretty = TRUE)
}
