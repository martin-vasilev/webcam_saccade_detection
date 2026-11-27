# New input/checkpoint adapter; historical get_xy and six detector expressions are sourced unchanged.
replay_detectors <- function(package,output,paths,scope) {
  dest<-file.path(output,'detector_replay');pin<-file.path(dest,'verification.json')
  if(file.exists(pin)) {
    m<-jsonlite::fromJSON(pin)
    if(m$scope==scope) {
      for(n in names(m$output_hashes))stopifnot(digest::digest(readBin(file.path(dest,n),'raw',n=file.info(file.path(dest,n))$size),algo='sha256',serialize=FALSE)==m$output_hashes[[n]])
      cat('Reused verified detector/preprocessing replay:',m$calls,'calls.\n');return(if(is.null(m$active_paths))NULL else unlist(m$active_paths))
    } else stop('Existing detector replay has a different execution scope; use a new private output root.')
  }
  source(paths[['historical_get_xy']]);source(paths[['detector_expressions']])
  grid<-read.csv(paths[['b_grid']]);trials<-read.csv(paths[['trials']]);truth<-read.csv(paths[['truth']])
  truth<-subset(truth,conf>0 & el_pupil>0)
  if(scope=='pilot')trials<-head(trials,2)
  records<-list();preprocessed<-list();emitted<-list()
  for(condition in c('original','rate30')) {
    archived<-read.csv(paths[[paste0('b_events_',condition)]])
    ledger<-read.csv(paths[[paste0('b_status_',condition)]])
    for(i in seq_len(nrow(trials))) {
      ss<-trials$sub[i];tt<-trials$Trial_Id[i]
      df0<-if(condition=='original')truth[truth$sub==ss & truth$Trial_Id==tt,] else read.csv(paths[[sprintf('coordinates_sub%d_trial%d',ss,tt)]])
      if(condition!='original')df0$ground_truth<-NA_character_
      stopifnot(all(diff(df0$time_start)>0),all(is.finite(df0$x)),all(is.finite(df0$y)))
      for(k in seq_len(nrow(grid))) {
        cfg<-grid[k,];warn<-character();err<-NA_character_;d<-NULL
        result<-tryCatch(withCallingHandlers({
          d<-get_xy(df0,cfg$smoothing)
          if(nrow(d)<5)NULL else {
            fun<-get(paste0('detect_original_',cfg$method,'_',cfg$event));z<-list(d=d,current_sub=ss,current_trial=tt,s=cfg$smoothing)
            if(cfg$method=='IVT')z$v<-cfg$threshold_value
            if(cfg$method=='IDT')z$disp<-cfg$threshold_value
            if(cfg$method=='EK03')z$l<-cfg$threshold_value
            do.call(fun,z)
          }
        },warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')}),error=function(e){err<<-conditionMessage(e);NULL})
        ne<-if(is.null(result))0L else nrow(result)
        state<-if(is.null(d))'preprocessing_error' else if(nrow(d)<5)'insufficient_samples' else if(!is.na(err))'error' else if(ne==0)'success_empty' else 'success'
        old<-ledger[ledger$sub==ss & ledger$Trial_Id==tt & ledger$configuration_id==cfg$configuration_id,]
        if(nrow(old)!=1 || old$status!=state || old$n_events!=ne) {
          jsonlite::write_json(list(condition=condition,sub=ss,trial=tt,configuration=cfg$configuration_id,archived_status=old$status,actual_status=state,archived_events=old$n_events,actual_events=ne,error=err,warnings=warn),file.path(output,'validation/detector_replay_discrepancy.json'),auto_unbox=TRUE,pretty=TRUE)
          stop('Detector replay differs: ',condition,' ',ss,'/',tt,' ',cfg$configuration_id,' ',state,' (',err,'); archived ',old$status)
        }
        if(ne>0) {
          start<-result[[if(cfg$event=='fixation')'fix_start' else 'sacc_start']];end<-result[[if(cfg$event=='fixation')'fix_end' else 'sacc_end']]
          ref<-archived[archived$sub==ss & archived$Trial_Id==tt & archived$configuration_id==cfg$configuration_id,c('start_ms','end_ms','duration')]
          stopifnot(isTRUE(all.equal(unname(as.matrix(ref)),unname(cbind(start,end,result$duration)),tolerance=1e-10,check.attributes=FALSE)))
          emitted[[length(emitted)+1]]<-data.frame(condition,sub=ss,Trial_Id=tt,configuration_id=cfg$configuration_id,start_ms=start,end_ms=end,duration=result$duration)
        }
        records[[length(records)+1]]<-data.frame(condition,sub=ss,Trial_Id=tt,configuration_id=cfg$configuration_id,status=state,n_events=ne,smoothing_fallback=cfg$smoothing=='sg' && nrow(df0)<23,agrees_with_archived=TRUE)
        if(k%in%which(!duplicated(grid$smoothing)))preprocessed[[length(preprocessed)+1]]<-data.frame(condition,sub=ss,Trial_Id=tt,smoothing=cfg$smoothing,d)
      }
    }
  }
  dest<-file.path(output,'detector_replay');dir.create(dest,showWarnings=FALSE)
  write.csv(do.call(rbind,records),file.path(dest,'execution_status.csv'),row.names=FALSE,na='')
  write.csv(do.call(rbind,preprocessed),file.path(dest,'smoothed_coordinates.csv'),row.names=FALSE,na='')
  write.csv(do.call(rbind,emitted),file.path(dest,'events.csv'),row.names=FALSE,na='')
  files<-c('execution_status.csv','smoothed_coordinates.csv','events.csv')
  hashes<-setNames(vapply(file.path(dest,files),function(p)digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE),character(1)),files)
  active_paths<-list()
  if(scope=='full') {
    event_table<-do.call(rbind,emitted);status_table<-do.call(rbind,records)
    for(condition in c('original','rate30')) {
      ep<-file.path(dest,paste0('events_',condition,'.csv'));sp<-file.path(dest,paste0('status_',condition,'.csv'))
      write.csv(event_table[event_table$condition==condition,],ep,row.names=FALSE)
      write.csv(status_table[status_table$condition==condition,],sp,row.names=FALSE)
      active_paths[[paste0('b_events_',condition)]]<-ep;active_paths[[paste0('b_status_',condition)]]<-sp
      for(p in c(ep,sp))hashes[basename(p)]<-digest::digest(readBin(p,'raw',n=file.info(p)$size),algo='sha256',serialize=FALSE)
    }
  }
  jsonlite::write_json(list(scope=scope,calls=length(records),output_hashes=as.list(hashes),active_paths=active_paths),pin,auto_unbox=TRUE,pretty=TRUE)
  cat('Historical preprocessing/detection replay:',length(records),'calls exactly matched archived events/statuses.\n')
  unlist(active_paths)
}
