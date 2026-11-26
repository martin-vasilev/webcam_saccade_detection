args <- commandArgs(FALSE);p <- sub('^--file=','',args[grepl('^--file=',args)])
root <- normalizePath(file.path(dirname(p),'../..'),winslash='/');w<-normalizePath(file.path(root,'../..'),winslash='/')
.libPaths(c(file.path(w,'classical_detection_reproduction/R_library'),file.path(w,'classical_evaluation_reproduction/R_library'),.libPaths()))
suppressPackageStartupMessages(library(dplyr))
sys.source(file.path(w,'classical_evaluation_reproduction/source_extracts/original_wasserstein_functions.R'),envir=environment())
x <- matrix(1,nrow=3,ncol=3);z <- compute_wasserstein_full(x,x+1)
stopifnot(abs((1/z$W_multi-1)-sqrt(3))<1e-10,abs((1/z$W_n-1)-1)<1e-10)
short <- compute_wasserstein_full(x[1:2,,drop=FALSE],x[1:2,,drop=FALSE]);stopifnot(all(is.na(unlist(short))))
dat <- data.frame(sub=1,Trial_Id=1,time_start=seq(0,60,10),ground_truth=c('fixation','fixation',NA,'fixation','saccade','fixation','fixation'))
sys.source(file.path(w,'classical_evaluation_reproduction/source_extracts/original_rmd_reference_events.R'),envir=environment())
stopifnot(nrow(gt_fixation_events)==2,identical(sort(gt_fixation_events$duration),c(10,30)))
dir.create(file.path(root,'output/part3/validation'),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(list(passed=TRUE,constant_reference_zero_scale_distance=1/z$W_multi-1,
  fewer_than_three_trials_returns_NA=TRUE,legacy_NA_propagation_fixture_demonstrated=TRUE,
  fixture_fixation_start=gt_fixation_events$fix_start,fixture_fixation_end=gt_fixation_events$fix_end),
  file.path(root,'output/part3/validation/extra_fixtures.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Constant-scale, insufficient-trial and literal NA-run reference fixtures PASS.\n')
