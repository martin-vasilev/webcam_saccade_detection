#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
script <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)[1])
root <- dirname(normalizePath(script, winslash = '/'))
workspace <- normalizePath(file.path(root, '../..'), winslash = '/')
.libPaths(c(file.path(workspace, 'classical_detection_reproduction/R_library'), file.path(workspace, 'classical_evaluation_reproduction/R_library'), .libPaths()))
source(file.path(root, 'R/inputs.R'))
position <- match('--stage', args)
stage <- if (is.na(position)) 'part1' else args[position + 1]
if (!stage %in% c('part1', 'part2', 'part2b', 'verify-inputs')) stop('Supported stages: part1, part2, part2b, verify-inputs')
if (stage %in% c('part1', 'verify-inputs')) {
  config <- jsonlite::fromJSON(file.path(root, 'config/protocol.json'))
  invisible(verify_inputs(root, config))
  if (stage == 'part1') {
    destination <- file.path(root, 'output/part1')
    manifest <- jsonlite::fromJSON(file.path(destination, 'output_manifest.json'))
    stopifnot(isTRUE(manifest$passed))
    for (name in names(manifest$outputs)) stopifnot(file_hash(file.path(destination, name)) == manifest$outputs[[name]])
    for (name in names(manifest$implementation)) {
      path <- if (name == 'run_results.R') file.path(root, 'validation/part2/run_results_part1_v1.R') else file.path(root, name)
      stopifnot(file_hash(path) == manifest$implementation[[name]])
    }
    cat('Part 1 outputs and archived original implementation verified; reused.\n')
  } else cat('Part 1 frozen input identities verified.\n')
  quit(status = 0)
}
for (name in c('inputs', 'selection', 'evaluation', 'figures', 'workflow')) source(file.path(root, 'R/part2', paste0(name, '.R')))
if (stage == 'part2') {
  invisible(part2_inputs(root))
  destination <- file.path(root, 'output/part2')
  manifest <- jsonlite::fromJSON(file.path(destination, 'output_manifest.json'))
  stopifnot(isTRUE(manifest$passed))
  for (name in names(manifest$outputs)) stopifnot(file_hash(file.path(destination, name)) == manifest$outputs[[name]])
  for (name in names(manifest$implementation)) {
    path <- if (name == 'run_results.R') file.path(root, 'validation/part2b/run_results_part2_v2.R') else file.path(root, name)
    stopifnot(file_hash(path) == manifest$implementation[[name]])
  }
  cat('Part 2 completed outputs, frozen inputs and archived version-2 entry point verified; reused.\n')
} else {
  source(file.path(root,'R/part2b/workflow.R'))
  run_part2b(root,workspace)
}
