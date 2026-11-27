part3_transport <- function(context,root,out,paths) {
  workspace <- normalizePath(file.path(root,'../..'),winslash='/')
  transport_lib <- file.path(workspace,'classical_evaluation_reproduction/R_library')
  .libPaths(c(file.path(workspace,'classical_detection_reproduction/R_library'),transport_lib,.libPaths()))
  suppressPackageStartupMessages(library(T4transport,lib.loc=transport_lib))
  sys.source(paths[['original_wasserstein_functions.R']],envir=environment())
  # Windows metadata-access checks in packageVersion() can reject this sandbox
  # despite a successful namespace load. Verify the loaded, byte-pinned package.
  transport_version <- as.character(getNamespaceVersion('T4transport'))
  stopifnot(transport_version=='0.1.8')
  x <- matrix(rep(c(0,10,20),3),ncol=3)
  fixture_identity <- compute_wasserstein_full(x,x)
  fixture_shift <- compute_wasserstein_full(x,sweep(x,2,c(3,4,0),'+'))
  stopifnot(max(abs(unlist(fixture_identity)-1))<1e-10,abs(fixture_shift$W_multi-1/1.5)<1e-10)
  one <- T4transport::wasserstein(matrix(c(0,2,4),ncol=1),matrix(c(1,3,5),ncol=1),p=1)$distance
  stopifnot(abs(one-1)<1e-10)
  jsonlite::write_json(list(passed=TRUE,identity_similarity=unlist(fixture_identity),standardised_shift_distance=1/fixture_shift$W_multi-1,
    univariate_translation_distance=one,package_version=transport_version),file.path(out,'validation/transport_fixtures.json'),auto_unbox=TRUE,pretty=TRUE)
  trials <- read.csv(paths[['trials']]);trial_key <- paste(trials$sub,trials$Trial_Id,sep='/')
  hash <- function(k)digest::digest(paste(trial_key[k],collapse=','),algo='sha256',serialize=FALSE)
  models <- c('IVT','IDT','EK03','IHMM','CNN_raw','CNN_mean','CNN_median','CNN_sg_p3_n7','CNN_sg_p5_n23')
  primary <- models[1:5];historical <- list();harmonised <- list();ledger <- list();scales <- list()
  cache <- new.env(parent=emptyenv())
  distance <- function(g,a,compatibility=FALSE) {
    cache_key <- digest::digest(list(g,a,compatibility),algo='sha256')
    if(exists(cache_key,cache,inherits=FALSE))return(get(cache_key,cache))
    centre <- colMeans(g);scale <- apply(g,2,sd);scale[scale==0] <- 1
    gs <- scale(g,center=centre,scale=scale);as <- scale(a,center=centre,scale=scale)
    if(compatibility) {
      score <- compute_wasserstein_full(g,a)
      similarity <- unlist(score[c('W_multi','W_n','W_mean','W_sd')],use.names=FALSE)
      d <- 1/similarity-1
    } else {
      d <- c(T4transport::wasserstein(gs,as,p=1)$distance,
        vapply(1:3,function(j)mean(abs(sort(gs[,j])-sort(as[,j]))),numeric(1)))
      similarity <- 1/(1+d)
    }
    result <- c(setNames(d,c('distance_multi','distance_N','distance_Mean','distance_SD')),
      setNames(similarity,c('similarity_multi','similarity_N','similarity_Mean','similarity_SD')),
      distance_N_native_units=mean(abs(sort(g[,1])-sort(a[,1]))),
      distance_Mean_ms=mean(abs(sort(g[,2])-sort(a[,2]))),distance_SD_ms=mean(abs(sort(g[,3])-sort(a[,3]))))
    assign(cache_key,result,cache);result
  }
  for(ref in c('historical','gap_aware')) for(variant in c('native','span')) for(event in c('fixation','saccade')) {
    s <- context$summary[context$summary$duration_variant==variant & context$summary$event==event,]
    retrieve <- function(model,condition) {
      d <- s[s$model==model & s$representation==condition & s$reference_definition==if(model=='GT')ref else 'native',]
      stopifnot(nrow(d)==792)
      d[match(trial_key,paste(d$sub,d$Trial_Id,sep='/')),]
    }
    gt <- retrieve('GT','original');g <- as.matrix(gt[c('N','Mean','SD')])
    choices <- setNames(lapply(models,function(m)setNames(lapply(c('original','rate30'),function(c)retrieve(m,c)),c('original','rate30'))),models)
    complete <- function(d)d$available & complete.cases(d[c('N','Mean','SD')])
    common <- complete(gt);available <- gt$available
    for(m in primary)for(c in c('original','rate30')) {
      common <- common & complete(choices[[m]][[c]])
      available <- available & choices[[m]][[c]]$available
    }
    emit <- function(model,condition,keep,policy,compatibility=FALSE,count_only=FALSE) {
      d <- choices[[model]][[condition]];a <- as.matrix(d[c('N','Mean','SD')])
      id <- paste(ref,variant,event,policy,model,condition,sep=':')
      centre <- if(sum(keep)>0)colMeans(g[keep,,drop=FALSE],na.rm=TRUE) else rep(NA_real_,3)
      scale_raw <- if(sum(keep)>1)apply(g[keep,,drop=FALSE],2,sd,na.rm=TRUE) else rep(NA_real_,3)
      scale <- scale_raw;scale[is.finite(scale)&scale==0] <- 1
      if(count_only) {centre[2:3]<-NA_real_;scale[2:3]<-NA_real_}
      scales[[length(scales)+1]] <<- data.frame(comparison_id=id,support_sha256=hash(keep),n_trials=sum(keep),
        N_centre=centre[1],Mean_centre_ms=centre[2],SD_centre_ms=centre[3],N_scale=scale[1],Mean_scale_ms=scale[2],SD_scale_ms=scale[3])
      values <- setNames(rep(NA_real_,11),c('distance_multi','distance_N','distance_Mean','distance_SD',
        'similarity_multi','similarity_N','similarity_Mean','similarity_SD','distance_N_native_units','distance_Mean_ms','distance_SD_ms'))
      if(sum(keep)>=3) {
        if(count_only) {
          values['distance_N_native_units'] <- mean(abs(sort(g[keep,1])-sort(a[keep,1])))
          values['distance_N'] <- values['distance_N_native_units']/scale[1]
          values['similarity_N'] <- 1/(1+values['distance_N'])
        } else values <- distance(g[keep,,drop=FALSE],a[keep,,drop=FALSE],compatibility)
      }
      reason <- ifelse(keep,'included',ifelse(!gt$available,'reference_unavailable',ifelse(!d$available,'method_call_unavailable',
        ifelse(!count_only & !complete(gt),'reference_zero_or_singleton',ifelse(!count_only & !complete(d),'method_zero_or_singleton','excluded_by_other_primary_method_or_paired_rate')))))
      ledger[[length(ledger)+1]] <<- data.frame(comparison_id=id,trials,support_policy=policy,included=keep,exclusion_reason=reason)
      row <- data.frame(comparison_id=id,model=model,representation=condition,event=event,reference_definition=ref,
        duration_variant=variant,support_policy=policy,n_trials=sum(keep),excluded_trials=792-sum(keep),support_sha256=hash(keep),
        status=if(sum(keep)>=3)'defined' else 'insufficient_complete_trials',as.list(values),check.names=FALSE)
      if(compatibility)historical[[length(historical)+1]] <<- row else harmonised[[length(harmonised)+1]] <<- row
    }
    for(model in models)for(condition in c('original','rate30')) {
      d <- choices[[model]][[condition]]
      emit(model,condition,complete(gt)&complete(d),'historical_method_specific_complete',TRUE)
      if(model %in% primary)keep<-common else {
        keep<-common & complete(choices[[model]][['original']]) & complete(choices[[model]][['rate30']])
      }
      emit(model,condition,keep,if(model %in% primary)'primary_five_common_complete' else 'CNN_sensitivity_paired_branch_complete')
      if(variant=='native')emit(model,condition,available & choices[[model]][['original']]$available & choices[[model]][['rate30']]$available,'count_only_common_available',count_only=TRUE)
    }
    cat('Transport completed:',ref,variant,event,'common complete trials',sum(common),'\n');flush.console()
  }
  hist <- do.call(rbind,historical);harm <- do.call(rbind,harmonised)
  method <- hist;method$support_policy <- 'method_specific_complete_sensitivity_same_as_historical'
  # The explicit method-specific sensitivity is numerically the compatibility result,
  # not another differently scaled calculation or another transport solver call.
  harm <- rbind(harm,method)
  write.csv(hist,file.path(out,'wasserstein_historical_compatibility.csv'),row.names=FALSE,na='')
  write.csv(harm,file.path(out,'wasserstein_harmonised.csv'),row.names=FALSE,na='')
  write.csv(do.call(rbind,ledger),file.path(out,'complete_case_ledger.csv'),row.names=FALSE,na='')
  write.csv(do.call(rbind,scales),file.path(out,'reference_scaling.csv'),row.names=FALSE,na='')
  list(historical=hist,harmonised=harm,ledger=do.call(rbind,ledger))
}
