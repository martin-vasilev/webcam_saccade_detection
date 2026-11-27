read_frozen <- function(root, stage, file) read.csv(file.path(root, 'data/frozen', stage, file), stringsAsFactors = FALSE, check.names = FALSE)
assert_near <- function(a,b, context) {
  stopifnot(length(a)==length(b), identical(is.na(a),is.na(b)))
  if (any(abs(a-b)>1e-11, na.rm=TRUE)) stop('Numerical identity failed: ', context)
}
verify_frozen <- function(root, stage) {
  m <- read.csv(file.path(root,'config/frozen_manifest.csv'), stringsAsFactors=FALSE)
  m <- m[m$stage %in% c(stage,'source'),]
  for (i in seq_len(nrow(m))) {
    p <- file.path(root,m$path[i])
    # Hash bytes directly: Windows sandbox file.access() may deny metadata while reads work.
    if (!file.exists(p) || digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE)!=m$sha256[i]) stop('Frozen hash mismatch: ',m$path[i])
  }
  metrics_checked <- 0L
  for (f in m$path[grepl('\\.csv$',m$path)]) {
    x <- read.csv(file.path(root,f),check.names=FALSE,stringsAsFactors=FALSE)
    if (all(c('TP','TN','FP','FN','MCC') %in% names(x))) {
      z <- t(apply(x[,c('TP','TN','FP','FN')],1,classification_metrics))
      for (metric in intersect(colnames(z),names(x))) assert_near(z[,metric],x[[metric]],paste(f,metric))
      for (den in intersect(c('scored_rows','evaluation_rows'),names(x))) assert_near(rowSums(x[,c('TP','TN','FP','FN')]),x[[den]],paste(f,den))
      metrics_checked <- metrics_checked+nrow(x)
    }
    for (metric in c('multi','N','Mean','SD')) {
      d <- paste0('distance_',metric); s <- paste0('similarity_',metric)
      if(all(c(d,s)%in%names(x))) assert_near(1/(1+x[[d]]),x[[s]],paste(f,s))
    }
  }
  if(stage=='part1') {
    grid <- read_frozen(root,stage,'grid_definitions_source.csv')
    stopifnot(nrow(grid)==320,!anyDuplicated(grid$configuration_id),all(grid$SG_window_samples[grid$smoothing=='sg']==23))
  }
  if(stage=='part3') {
    w <- read_frozen(root,stage,'wasserstein_harmonised.csv')
    z <- w[w$support_policy=='primary_five_common_complete' & w$reference_definition=='historical' & w$duration_variant=='native',]
    stopifnot(all(z$n_trials[z$event=='fixation']==516),all(z$n_trials[z$event=='saccade']==366))
    for(e in unique(z$event))stopifnot(length(unique(z$support_sha256[z$event==e]))==1)
    v <- read_frozen(root,stage,'saccade_velocity_summary.csv')
    stopifnot(all(v$events==v$defined_events+v$undefined_events))
  }
  list(passed=TRUE,hashed_files=nrow(m),confusion_rows_checked=metrics_checked)
}
record_environment <- function(root) {
  dir.create(file.path(root,'environment'),showWarnings=FALSE,recursive=TRUE)
  writeLines(capture.output(sessionInfo()),file.path(root,'environment/sessionInfo.txt'))
  v <- data.frame(package=c('R','jsonlite','digest'),version=c(paste(R.version$major,R.version$minor,sep='.'),
    as.character(getNamespaceVersion('jsonlite')),as.character(getNamespaceVersion('digest'))))
  write.csv(v,file.path(root,'environment/versions.csv'),row.names=FALSE)
}
