#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep('^--file=', commandArgs(), value = TRUE)
root <- dirname(normalizePath(sub('^--file=', '', script_arg[1]), winslash = '/'))
workspace <- normalizePath(file.path(root, '../..'), winslash = '/')
.libPaths(c(file.path(workspace, 'classical_detection_reproduction/R_library'),
            file.path(workspace, 'classical_evaluation_reproduction/R_library'), .libPaths()))
option <- function(name, default) {
  pos <- match(name, args)
  if (is.na(pos)) return(default)
  if (pos == length(args)) stop('Missing value for ', name)
  args[pos + 1]
}
stage <- option('--stage', 'part1')
if (!stage %in% c('part1', 'verify-inputs')) stop('Supported stages are part1 and verify-inputs. Future stages require a new approved implementation.')
config_path <- option('--config', file.path(root, 'config/protocol.json'))
if (!file.exists(config_path)) stop('Configuration not found')
for (name in c('inputs', 'part1', 'figures')) source(file.path(root, 'R', paste0(name, '.R')))
config <- jsonlite::fromJSON(config_path)
stopifnot(config$version == 'martin_results_extension_001.part1.v1',
          identical(config$conditions, c('original', 'rate30')), config$stage == 'part1')
inputs <- verify_inputs(root, config)
if (stage == 'verify-inputs') {
  cat('All frozen input identities verified.\n')
  quit(status = 0)
}
destination <- file.path(root, 'output/part1')
if (dir.exists(destination)) {
  verify_completed(destination, root)
  cat('Existing completed Part 1 output hashes verified; reused.\n')
  quit(status = 0)
}
pending <- file.path(root, 'output', paste0('pending_', format(Sys.time(), '%Y%m%d_%H%M%S'), '_', Sys.getpid()))
dir.create(pending, recursive = TRUE)
for (name in c('tables', 'figures', 'validation')) dir.create(file.path(pending, name))
RNGkind('Mersenne-Twister', 'Inversion', 'Rejection')
set.seed(config$seed)
datasets <- import_part1(inputs, config)
write_tables(datasets, pending)
draw_figures(datasets, pending)
capture.output(sessionInfo(), file = file.path(pending, 'validation/sessionInfo.txt'))
packages <- data.frame(package = c('R', 'jsonlite', 'digest'),
                       version = c(as.character(getRversion()), as.character(packageVersion('jsonlite')), as.character(packageVersion('digest'))))
write.csv(packages, file.path(pending, 'validation/package_versions.csv'), row.names = FALSE)
python <- file.path(workspace, 'sampling_rate_analysis/extension_c/c3_cnn_retraining/py311/python.exe')
check <- system2(python, c('-B', shQuote(file.path(root, 'validation/independent_check.py')),
                          shQuote(root), shQuote(pending)), stdout = TRUE, stderr = TRUE)
writeLines(check, file.path(pending, 'validation/independent_check.log'))
if (!is.null(attr(check, 'status')) && attr(check, 'status') != 0) stop('Independent validation failed; pending outputs retained')
invisible(verify_inputs(root, config))
writeLines(c('# Phase 1 status', '', 'Numerical validation PASS. Visual review pending.',
             'All outputs derive from frozen tables. See figure captions and source tables for distinct supports.'), file.path(pending, 'PHASE1_STATUS.md'))
if (!file.rename(pending, destination)) stop('Could not commit output directory')
cat('Part 1 numerical validation passed. Outputs await visual review in ', destination, '\n', sep = '')
