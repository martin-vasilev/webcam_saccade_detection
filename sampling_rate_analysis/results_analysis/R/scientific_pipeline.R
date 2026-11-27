# Portable orchestration only. Scientific functions are sourced from executable/.
scientific_run <- function(package, stage, args, option) {
  input <- option('input-root', '')
  output <- option('output-root', '')
  python <- option('python', Sys.getenv('PYTHON', 'python'))
  scope <- option('transport-scope','full')
  detector_scope <- option('detector-scope','pilot')
  if(!nzchar(input)||!nzchar(output))stop('recompute requires --input-root and --output-root outside the public package; see INPUT_DATA.md. No aggregate fallback.')
  stopifnot(scope %in% c('representative','full'),detector_scope %in% c('pilot','full','archived'))
  input <- normalizePath(input,winslash='/',mustWork=TRUE)
  safe<-system2(python,c('-B',shQuote(file.path(package,'python/validate_output.py')),shQuote(package),shQuote(input),shQuote(output)),stdout=TRUE,stderr=TRUE)
  if(!is.null(attr(safe,'status'))&&attr(safe,'status')!=0)stop('Unsafe scientific output path: ',paste(safe,collapse=' '))
  output<-jsonlite::fromJSON(paste(safe,collapse='\n'))$output
  output <- gsub('\\','/',path.expand(output),fixed=TRUE)
  dir.create(output,recursive=TRUE,showWarnings=FALSE);output<-normalizePath(output,winslash='/',mustWork=TRUE)
  if(startsWith(tolower(paste0(output,'/')),tolower(paste0(package,'/'))))stop('Scientific outputs cannot be written into the proposed public release (participant-level data).')
  forbidden<-c('martin_results_extension_001','analysis_b/runs','full_analysis_a_001','extension_c','downstream_original_vs_30hz_001')
  for(x in forbidden)if(startsWith(tolower(output),tolower(file.path(input,'sampling_rate_analysis',x))))stop('Protected scientific output location rejected: ',output)
  for(p in c('dplyr','purrr','tidyr','tibble','signal','eyemovements','T4transport'))if(!requireNamespace(p,quietly=TRUE))stop('Scientific dependency missing: ',p)
  suppressPackageStartupMessages({library(dplyr);library(purrr);library(tidyr);library(tibble)})
  scientific_code<-jsonlite::fromJSON(file.path(package,'config/executable_code_manifest.json'))
  for(i in seq_len(nrow(scientific_code))) {
    p<-file.path(package,scientific_code$path[i])
    stopifnot(digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE)==scientific_code$release_sha256[i])
  }
  for(f in c('part2/inputs.R','part2/selection.R','part2/evaluation.R','part2b/workflow.R','part2b/calculate.R','part3/events.R','part3/velocities.R','part3/transport.R','part3/figures.R'))source(file.path(package,'R/executable',f))
  for(d in c('logs','validation','config/part3','output/part2/tables','output/part2b','output/part3/derived','output/part3/validation','output/part3/figures'))dir.create(file.path(output,d),recursive=TRUE,showWarnings=FALSE)
  invoke<-function(script,extra=character()) {
    log<-file.path(output,'logs',paste0(basename(script),'.log'))
    code<-system2(python,c('-B',shQuote(file.path(package,'python',script)),extra),stdout=log,stderr=log)
    if(code!=0)stop('Python scientific step failed: ',script,'; inspect ',log)
    cat('Executed',script,'\n');flush.console()
  }
  invoke('scientific_inputs.py',shQuote(c(package,input,output)))
  paths<-unlist(jsonlite::fromJSON(file.path(output,'input_paths.json')))
  file.copy(paths[['trials']],file.path(output,'config/part3/trials.csv'),overwrite=TRUE)
  source(file.path(package,'R/detector_replay.R'))
  done<-list();stages<-if(stage=='all')c('part1','part2','part2b','part3')else stage
  write_table<-function(x,path){dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);write.csv(x,path,row.names=FALSE,na='')}
  checkpoint<-function(name,files,details) {
    hashes<-setNames(vapply(file.path(output,files),function(p)digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE),character(1)),files)
    jsonlite::write_json(list(computed=TRUE,stage=name,details=details,output_hashes=as.list(hashes)),file.path(output,'validation',paste0(name,'_computation.json')),auto_unbox=TRUE,pretty=TRUE)
  }
  checkpoint_ready<-function(name) {
    p<-file.path(output,'validation',paste0(name,'_computation.json'))
    if(!file.exists(p))return(FALSE)
    m<-jsonlite::fromJSON(p)
    for(n in names(m$output_hashes))stopifnot(digest::digest(readBin(file.path(output,n),'raw',n=file.info(file.path(output,n))$size),algo='sha256',serialize=FALSE)==m$output_hashes[[n]])
    TRUE
  }
  if('part1'%in%stages) {
    prior<-file.path(output,'validation/part1_computation.json')
    if(file.exists(prior)&&jsonlite::fromJSON(prior)$details$detector_execution!=detector_scope)stop('Detector execution scope differs from this run checkpoint; use a new private output root.')
    if(!checkpoint_ready('part1')) {
    if(detector_scope!='archived') {
      active<-replay_detectors(package,output,paths,detector_scope)
      if(length(active))paths[names(active)]<-active
    }
    jsonlite::write_json(as.list(paths),file.path(output,'active_paths.json'),auto_unbox=TRUE,pretty=TRUE)
    invoke('recompute_counts.py',shQuote(output))
    paths<-unlist(jsonlite::fromJSON(file.path(output,'recomputed_paths.json')))
    checkpoint('part1',c('derived_counts/b_counts.csv.gz','derived_counts/h_counts.csv.gz','derived_counts/cnn_counts.csv','derived_counts/part1_landscapes.csv'),list(scoring='full Original/30-Hz event grid and all five CNN branches',detector_execution=detector_scope,three_rate_status_support=TRUE))
    } else {paths<-unlist(jsonlite::fromJSON(file.path(output,'recomputed_paths.json')));cat('Reused hash-verified actual Part 1 recomputation checkpoint.\n')}
    done$part1<-'full native-event and prediction scoring; detector replay scope recorded separately'
    if(detector_scope!='archived')invoke('independent_preprocessing.py',shQuote(output))
  } else if(file.exists(file.path(output,'recomputed_paths.json')))paths<-unlist(jsonlite::fromJSON(file.path(output,'recomputed_paths.json')))
  context<-NULL
  registry<-data.frame(id=names(paths),path=unname(paths),sha256=vapply(unname(paths),function(p)digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE),character(1)))
  write.csv(registry,file.path(output,'config/part2/input_manifest.csv'),row.names=FALSE)
  if(any(c('part2','part2b')%in%stages))context<-part2_context(paths)
  if('part2'%in%stages) {
    selection<-select_joint(context);stopifnot(all(selection$selections$outcome=='selected'))
    choices<-policy_calls(context,selection$selections,paths);results<-evaluate_policies(context,choices);cnn<-evaluate_cnn(paths)
    tables<-list(selected_configurations=selection$selections,training_support=selection$training,candidate_scores=selection$candidates,
      policy_configuration_choices=choices,heldout_trial_counts=results$trials,heldout_performance=results$pooled,
      heldout_participant_performance=results$participants,cnn_five_branch_performance=cnn)
    for(n in names(tables))write_table(tables[[n]],file.path(output,'output/part2/tables',paste0(n,'.csv')))
    dir.create(file.path(output,'output/part2/validation'),showWarnings=FALSE)
    invoke('independent_selection.py',shQuote(c(output,file.path(output,'output/part2'))))
    checkpoint('part2',paste0('output/part2/tables/',names(tables),'.csv'),list(outer_folds=20,selection_count=nrow(selection$selections),training_only=TRUE))
    done$part2<-'all 160 LOPO joint choices and all held-out policy/branch metrics recomputed'
  }
  if('part2b'%in%stages) {
    if(!file.exists(file.path(output,'output/part2/tables/selected_configurations.csv')))stop('part2b needs recomputed part2 in the same --output-root; run part2 first.')
    calculate_part2b(context,output,file.path(output,'output/part2b'))
    checkpoint('part2b',c('output/part2b/final_fixed_configurations.csv','output/part2b/configuration_stability.csv','output/part2b/performance_interpretation.csv'),list(full_development_selection=TRUE,CNN_primary='Raw; not reselected'))
    done$part2b<-'all eight fixed settings, stability and development metrics recomputed'
  }
  if('part3'%in%stages) {
    prior<-file.path(output,'validation/part3_computation.json')
    if(file.exists(prior)&&jsonlite::fromJSON(prior)$details$transport_scope!=scope)stop('Transport scope differs from this run checkpoint; use a new private output root.')
    if(!file.exists(file.path(output,'output/part2b/final_fixed_configurations.csv')))stop('part3 needs recomputed part2b in the same --output-root.')
    pre_files<-c(paste0('output/part3/derived/',c('native_events.csv','call_roster.csv','coordinates.csv','cnn_original_coordinates.csv','trial_recording_periods.csv')),
      paste0('output/part3/',c('event_trial_summary.csv','event_distribution_summary.csv','event_availability_and_exclusions.csv','reference_event_validation.csv','saccade_velocity_events.csv','saccade_velocity_summary.csv','saccade_velocity_missingness.csv')))
    pre_gate<-file.path(output,'validation/part3_pretransport_computation.json')
    byte_sha<-function(p)digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE)
    pre_identity<-list(source_catalog_sha256=byte_sha(file.path(package,'config/scientific_inputs.csv')),code_manifest_sha256=byte_sha(file.path(package,'config/executable_code_manifest.json')),fixed_sha256=byte_sha(file.path(output,'output/part2b/final_fixed_configurations.csv')),input_root=input)
    if(file.exists(pre_gate)) {
      old<-jsonlite::fromJSON(pre_gate)$details
      for(n in names(pre_identity))if(!identical(old[[n]],pre_identity[[n]]))stop('Native/velocity checkpoint input/code identity changed; use a new private run root.')
    }
    if(checkpoint_ready('part3_pretransport')) {
      context<-list(events=read.csv(file.path(output,pre_files[1])),roster=read.csv(file.path(output,pre_files[2])),coordinates=read.csv(file.path(output,pre_files[3])),cnn_original_coordinates=read.csv(file.path(output,pre_files[4])),summary=read.csv(file.path(output,'output/part3/event_trial_summary.csv')))
      velocity<-read.csv(file.path(output,'output/part3/saccade_velocity_events.csv'))
      cat('Reused independently verified actual native/reference/velocity checkpoint.\n')
    } else {
      invoke('native_inputs.py',shQuote(output))
      context<-part3_events(output,file.path(output,'output/part3'),paths)
      velocity<-part3_velocities(context,output,file.path(output,'output/part3'),paths)
      invoke('independent_events_transport.py',c(shQuote(output),'--pretransport'))
      checkpoint('part3_pretransport',pre_files,c(list(independently_verified=TRUE),pre_identity))
    }
    transport_comparison_filter <<- if(scope=='representative')c(
      'historical:native:fixation:historical_method_specific_complete:IVT:original',
      'historical:native:fixation:primary_five_common_complete:IVT:original',
      'historical:native:fixation:count_only_common_available:IVT:original',
      'historical:native:saccade:primary_five_common_complete:IVT:original')else NULL
    transport<-part3_transport(context,output,file.path(output,'output/part3'),paths)
    invoke('independent_transport.py',shQuote(output))
    if(scope=='full') {
      part3_figures(context,transport,velocity,file.path(output,'output/part3'))
      invoke('event_sensitivities.py',shQuote(output))
    }
    checkpoint('part3',c('output/part3/event_distribution_summary.csv','output/part3/saccade_velocity_events.csv','output/part3/wasserstein_harmonised.csv'),list(native_events=nrow(context$events),velocity_records=nrow(velocity),transport_scope=scope,full_figure_inputs_recomputed=scope=='full'))
    done$part3<-paste('all native/reference summaries and velocities recomputed; exact transport scope:',scope)
  }
  status_path<-file.path(output,'validation/run_status.json')
  if(file.exists(status_path)) {
    prior_status<-jsonlite::fromJSON(status_path,simplifyVector=FALSE)
    done<-modifyList(prior_status$stages,done)
    if(!'part3'%in%stages)scope<-prior_status$transport_scope
    if(!'part1'%in%stages)detector_scope<-prior_status$detector_scope
  }
  jsonlite::write_json(list(stages=done,transport_scope=scope,detector_scope=detector_scope,scientific_outputs_private=TRUE,last_requested_stage=stage),status_path,pretty=TRUE,auto_unbox=TRUE)
  invoke('compare_scientific.py',shQuote(c(package,output)))
  capture.output(sessionInfo(),file=file.path(output,'validation/sessionInfo.txt'))
  cat('Scientific calculations finished at',output,'\nIndependent frozen comparison is separate; no cached scientific aggregate fallback.\n')
}
