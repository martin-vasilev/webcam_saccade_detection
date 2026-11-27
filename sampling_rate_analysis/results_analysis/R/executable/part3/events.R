part3_events <- function(root,out,paths) {
  ev <- read.csv(file.path(out,'derived/native_events.csv'),stringsAsFactors=FALSE)
  roster <- read.csv(file.path(out,'derived/call_roster.csv'),stringsAsFactors=FALSE)
  roster$available <- roster$available %in% c(TRUE,'True','TRUE')
  coord <- read.csv(file.path(out,'derived/coordinates.csv'))
  # Execute only the exact historical reference extraction/summaries, not detectors.
  # The original readr reader repaired the unnamed CSV row-index columns.
  # Base-R name repair gives the same usable scientific column names.
  dat <- read.csv(paths[['truth']],check.names=TRUE)
  dat <- subset(dat,conf>0 & el_pupil>0)
  scope <- new.env(parent=globalenv());scope$dat <- dat
  sys.source(paths[['original_rmd_reference_events.R']],envir=scope)
  for(event in c('fixation','saccade')) {
    historical <- scope[[paste0('gt_',event,'_events')]]
    actual <- ev[ev$model=='GT' & ev$representation=='original' & ev$reference_definition=='historical' & ev$event==event,]
    a <- as.data.frame(historical)[c('sub','Trial_Id',paste0(if(event=='fixation')'fix' else 'sacc','_start'),paste0(if(event=='fixation')'fix' else 'sacc','_end'),'duration')]
    names(a) <- c('sub','Trial_Id','start_ms','last_sample_ms','native_duration_ms')
    b <- actual[names(a)]
    a <- a[order(a$sub,a$Trial_Id,a$start_ms),];b <- b[order(b$sub,b$Trial_Id,b$start_ms),]
    stopifnot(nrow(a)==nrow(b),isTRUE(all.equal(as.matrix(a),as.matrix(b),check.attributes=FALSE,tolerance=1e-10)))
  }
  keys <- c('model','representation','reference_definition','event','sub','Trial_Id')
  views <- rbind(transform(ev,duration_variant='native',duration_ms=native_duration_ms),
                 transform(ev,duration_variant='span',duration_ms=span_ms))
  grouped <- views %>% group_by(across(all_of(c(keys,'duration_variant')))) %>%
    summarise(N=n(),Mean=mean(duration_ms),SD=sd(duration_ms),.groups='drop')
  full <- rbind(transform(roster,duration_variant='native'),transform(roster,duration_variant='span'))
  summary <- left_join(full,grouped,by=c(keys,'duration_variant'))
  stopifnot(all(is.na(summary$N[!summary$available])))
  summary$N[summary$available & is.na(summary$N)] <- 0L
  summary$summary_status <- ifelse(!summary$available,'call_unavailable',ifelse(summary$N==0,'successful_zero',ifelse(summary$N==1,'singleton_undefined_SD','complete_event_summary')))
  summary$complete_N_Mean_SD <- summary$available & complete.cases(summary[c('N','Mean','SD')])
  write.csv(summary,file.path(out,'event_trial_summary.csv'),row.names=FALSE,na='')
  distribution <- summary %>% group_by(across(all_of(c(keys[1:4],'duration_variant')))) %>%
    summarise(trials=n(),available_trials=sum(available),unavailable_trials=sum(!available),
      zero_event_trials=sum(N==0,na.rm=TRUE),singleton_trials=sum(N==1,na.rm=TRUE),
      N_denominator=sum(is.finite(N)),Mean_denominator=sum(is.finite(Mean)),SD_denominator=sum(is.finite(SD)),
      trial_N_mean=mean(N,na.rm=TRUE),trial_N_sample_SD=sd(N,na.rm=TRUE),
      trial_Mean_duration_mean_ms=mean(Mean,na.rm=TRUE),trial_Mean_duration_sample_SD_ms=sd(Mean,na.rm=TRUE),
      trial_SD_duration_mean_ms=mean(SD,na.rm=TRUE),trial_SD_duration_sample_SD_ms=sd(SD,na.rm=TRUE),.groups='drop')
  pooled <- views %>% group_by(across(all_of(c(keys[1:4],'duration_variant')))) %>%
    summarise(pooled_events=n(),pooled_event_duration_mean_ms=mean(duration_ms),pooled_event_duration_sample_SD_ms=sd(duration_ms),.groups='drop')
  distribution <- left_join(distribution,pooled,by=c(keys[1:4],'duration_variant'))
  distribution$scope <- 'all_successful_native_trials; event-pooled and equal-trial summaries separately labelled'
  write.csv(distribution,file.path(out,'event_distribution_summary.csv'),row.names=FALSE,na='')
  exclusion <- summary
  exclusion$exclusion_reason <- ifelse(!exclusion$available,'call_unavailable',ifelse(exclusion$N==0,'zero_events_mean_SD_undefined',ifelse(exclusion$N==1,'singleton_SD_undefined','complete')))
  write.csv(exclusion,file.path(out,'event_availability_and_exclusions.csv'),row.names=FALSE,na='')
  ref <- summary[summary$model=='GT' & summary$representation=='original' & summary$duration_variant=='native',]
  h <- ref[ref$reference_definition=='historical',c('sub','Trial_Id','event','N','Mean','SD')]
  g <- ref[ref$reference_definition=='gap_aware',c('sub','Trial_Id','event','N','Mean','SD')]
  validation <- merge(h,g,by=c('sub','Trial_Id','event'),suffixes=c('_historical','_gap_aware'))
  validation$historical_R_exact_match <- TRUE
  missing <- dat %>% group_by(sub,Trial_Id) %>% summarise(missing_reference_labels=sum(is.na(ground_truth)),.groups='drop')
  validation <- merge(validation,missing,by=c('sub','Trial_Id'))
  validation$historical_NA_event_id_propagation <- validation$missing_reference_labels>0
  periods <- read.csv(file.path(out,'derived/trial_recording_periods.csv'))
  validation <- merge(validation,periods,by=c('sub','Trial_Id'))
  write.csv(validation,file.path(out,'reference_event_validation.csv'),row.names=FALSE,na='')
  list(events=ev,views=views,roster=roster,summary=summary,coordinates=coord,
    cnn_original_coordinates=read.csv(file.path(out,'derived/cnn_original_coordinates.csv')))
}
