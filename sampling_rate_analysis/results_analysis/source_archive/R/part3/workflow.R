part3_inputs <- function(root) {
  protocol <- jsonlite::fromJSON(file.path(root,'config/part3/event_protocol.json'))
  registry_path <- file.path(root,'config/part3/input_manifest.csv')
  stopifnot(file_hash(registry_path)==protocol$input_manifest_sha256)
  registry <- read.csv(registry_path,stringsAsFactors=FALSE)
  for(i in seq_len(nrow(registry)))stopifnot(file_hash(registry$path[i])==registry$sha256[i])
  list(protocol=protocol,paths=setNames(registry$path,registry$id))
}
run_part3 <- function(root,workspace) {
  input <- part3_inputs(root);out <- file.path(root,'output/part3')
  if(file.exists(file.path(out,'output_manifest.json'))) {
    m <- jsonlite::fromJSON(file.path(out,'output_manifest.json'));stopifnot(isTRUE(m$passed))
    for(n in names(m$outputs))stopifnot(file_hash(file.path(out,n))==m$outputs[[n]])
    for(n in names(m$implementation))stopifnot(file_hash(file.path(root,n))==m$implementation[[n]])
    cat('Part 3 completed bundle, earlier outputs and scientific source hashes verified; reused.\n');return(invisible(NULL))
  }
  for(d in c('derived','figures','validation'))dir.create(file.path(out,d),recursive=TRUE,showWarnings=FALSE)
  file.copy(file.path(root,'config/part3/PART3_METHODS_DECISIONS.md'),file.path(out,'PART3_METHODS_DECISIONS.md'),overwrite=TRUE)
  python <- file.path(workspace,'sampling_rate_analysis/extension_c/c3_cnn_retraining/py311/python.exe')
  invoke <- function(script,log) {
    result <- system2(python,c('-B',shQuote(file.path(root,'validation/part3',script)),shQuote(root)),stdout=TRUE,stderr=TRUE)
    writeLines(result,file.path(out,'validation',log));cat(paste(result,collapse='\n'),'\n')
    if(!is.null(attr(result,'status'))&&attr(result,'status')!=0)stop(script,' failed; protected inputs unchanged')
  }
  gate <- function(name,files,implementation,write=FALSE) {
    p <- file.path(out,'validation',paste0(name,'_gate.json'))
    hashes <- setNames(vapply(file.path(out,files),function(p)if(file.exists(p))file_hash(p) else '',character(1)),files)
    current <- list(inputs_sha256=input$protocol$input_manifest_sha256,outputs=as.list(hashes),implementation=file_hash(implementation))
    if(write) {jsonlite::write_json(current,p,auto_unbox=TRUE,pretty=TRUE);return(TRUE)}
    if(!file.exists(p)||any(!nzchar(hashes)))return(FALSE)
    old <- jsonlite::fromJSON(p,simplifyVector=FALSE)
    identical(old,current)
  }
  prepare <- c('derived/native_events.csv','derived/call_roster.csv','derived/coordinates.csv','derived/trial_recording_periods.csv','derived/cnn_original_coordinates.csv')
  imp <- file.path(root,'validation/part3/prepare_inputs.py')
  if(!gate('native_input',prepare,imp)) {invoke('prepare_inputs.py','prepare_inputs.log');gate('native_input',prepare,imp,TRUE)}
  suppressPackageStartupMessages(library(dplyr))
  extra <- system2(file.path(R.home('bin'),'Rscript.exe'),shQuote(file.path(root,'validation/part3/extra_fixtures.R')),stdout=TRUE,stderr=TRUE)
  writeLines(extra,file.path(out,'validation/extra_fixtures.log'))
  if(!is.null(attr(extra,'status'))&&attr(extra,'status')!=0)stop('Additional historical fixture gate failed')
  summary_files <- c('event_trial_summary.csv','event_distribution_summary.csv','event_availability_and_exclusions.csv','reference_event_validation.csv')
  imp <- file.path(root,'R/part3/events.R')
  if(!gate('summary',summary_files,imp)) {
    context <- part3_events(root,out,input$paths);gate('summary',summary_files,imp,TRUE)
  } else {
    context <- list(events=read.csv(file.path(out,prepare[1])),roster=read.csv(file.path(out,prepare[2])),coordinates=read.csv(file.path(out,prepare[3])),summary=read.csv(file.path(out,'event_trial_summary.csv')),cnn_original_coordinates=read.csv(file.path(out,prepare[5])))
  }
  cat('Native/reference extraction and trial summaries validated.\n');flush.console()
  velocity_files <- c('saccade_velocity_events.csv','saccade_velocity_summary.csv','saccade_velocity_missingness.csv')
  imp <- file.path(root,'R/part3/velocities.R')
  if(!gate('velocity',velocity_files,imp)) {velocity <- part3_velocities(context,root,out,input$paths);gate('velocity',velocity_files,imp,TRUE)} else velocity <- read.csv(file.path(out,velocity_files[1]))
  cat('Supported historical and harmonised velocities reconstructed without event detection.\n');flush.console()
  transport_files <- c('wasserstein_historical_compatibility.csv','wasserstein_harmonised.csv','complete_case_ledger.csv','reference_scaling.csv','validation/transport_fixtures.json')
  imp <- file.path(root,'R/part3/transport.R')
  if(!gate('transport',transport_files,imp)) {transport <- part3_transport(context,root,out,input$paths);gate('transport',transport_files,imp,TRUE)} else transport <- list(historical=read.csv(file.path(out,transport_files[1])),harmonised=read.csv(file.path(out,transport_files[2])),ledger=read.csv(file.path(out,transport_files[3])))
  figure_names <- as.vector(outer(c('primary','CNN_sensitivity'),c('fixation_duration','saccade_duration','fixation_counts','saccade_counts','saccade_average_velocity','saccade_peak_velocity'),paste,sep='_'))
  figure_files <- c('figure_source_observations.csv','figure_source_curves.csv','figures/captions.json',paste0('figures/',rep(figure_names,each=2),rep(c('.pdf','.png'),length(figure_names))))
  imp <- file.path(root,'R/part3/figures.R')
  if(!gate('figures',figure_files,imp)) {part3_figures(context,transport,velocity,out);gate('figures',figure_files,imp,TRUE)}
  capture.output(sessionInfo(),file=file.path(out,'validation/sessionInfo.txt'))
  invoke('verify.py','independent.log')
  invisible(part3_inputs(root))
  cat('Part 3 numerical validation passed; final visual/export completion gate pending.\n')
}
