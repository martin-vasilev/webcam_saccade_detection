args <- commandArgs(trailingOnly = FALSE)
script <- sub('^--file=', '', args[grepl('^--file=', args)])
root <- normalizePath(file.path(dirname(script), '../..'), winslash = '/')
workspace <- normalizePath(file.path(root, '../..'), winslash = '/')
.libPaths(c(file.path(workspace, 'classical_detection_reproduction/R_library'),
            file.path(workspace, 'classical_evaluation_reproduction/R_library'), .libPaths()))
source(file.path(root, 'R/part2/figures.R'))
destination <- file.path(root, 'output/part2')
part2_figures(read.csv(file.path(destination, 'tables/heldout_performance.csv')),
              read.csv(file.path(destination, 'tables/cnn_five_branch_performance.csv')), destination)
cat('Figures regenerated from saved numeric tables only.\n')
