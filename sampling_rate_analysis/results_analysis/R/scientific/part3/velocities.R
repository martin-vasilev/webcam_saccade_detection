part3_velocities <- function(context,root,out,paths) {
  sys.source(paths[['historical_get_xy']],envir=environment())
  ev <- context$events[context$events$event=='saccade',]
  coords <- context$coordinates
  key <- function(condition,sub,trial)paste(condition,sub,trial,sep='/')
  groups <- split(coords,key(coords$representation,coords$sub,coords$Trial_Id))
  cg <- context$cnn_original_coordinates
  cnn_groups <- split(cg,key(cg$representation,cg$sub,cg$Trial_Id))
  pre <- read.csv(file.path(root,'output/part2b/final_fixed_configurations.csv'))
  result <- list();cache <- new.env(parent=emptyenv())
  scales <- c(.01865554,.01919689)
  for(i in seq_len(nrow(ev))) {
    e <- ev[i,];g <- groups[[key(e$representation,e$sub,e$Trial_Id)]]
    if(grepl('^CNN_',e$model)&&e$representation=='original')g<-cnn_groups[[key(e$representation,e$sub,e$Trial_Id)]]
    inside <- which(g$time_start >= e$start_ms-1e-7 & g$time_start <= e$last_sample_ms+1e-7)
    t <- g$time_start/1000
    if(length(inside)<2) {
      average <- peak <- NA_real_;reason <- 'insufficient_within_event_pairs';pairs <- blocked <- finite <- 0L
    } else {
      right <- inside[-1];left <- inside[-length(inside)]
      dt <- t[right]-t[left]
      speed <- sqrt(((g$x[right]-g$x[left])*scales[1])^2+((g$y[right]-g$y[left])*scales[2])^2)/dt
      blocked <- sum(g$component_id[right]!=g$component_id[left])
      valid <- is.finite(speed)&dt>0;pairs <- length(speed);finite <- sum(valid)
      reason <- if(blocked>0)'blocked_interval' else if(!all(valid))'invalid_coordinate_or_dt' else 'defined'
      average <- if(reason=='defined')mean(speed) else NA_real_;peak <- if(reason=='defined')max(speed) else NA_real_
    }
    result[[length(result)+1]] <- data.frame(event_id=e$event_id,model=e$model,representation=e$representation,
      reference_definition=e$reference_definition,sub=e$sub,Trial_Id=e$Trial_Id,velocity_rule='harmonised_raw_within_event',
      average_velocity_deg_s=average,peak_velocity_deg_s=peak,n_derivatives=pairs,n_finite_derivatives=finite,
      blocked_derivatives=blocked,reason=reason)
    if(!e$model %in% c('IVT','IDT','EK03','IHMM'))next
    setting <- pre[pre$algorithm==e$model & pre$representation==e$representation,]
    smooth <- c(Raw='raw',Mean3='mean',Median3='median',SG5_23='sg')[[setting$preprocessing]]
    ck <- paste(e$model,key(e$representation,e$sub,e$Trial_Id),sep='/')
    if(!exists(ck,envir=cache,inherits=FALSE)) {
      input <- data.frame(x=g$x,y=g$y,time_start=g$time_start,ground_truth=rep('unused',nrow(g)))
      d <- get_xy(input,smooth);n <- nrow(d);stopifnot(n==nrow(g))
      dt <- c(NA,diff(t));speed <- rep(NA_real_,n)
      if(e$model=='EK03') {
        for(j in 3:(n-2)) {
          vx <- (d$x[j+2]*scales[1]+d$x[j+1]*scales[1]-d$x[j-1]*scales[1]-d$x[j-2]*scales[1])/(1.5*(t[j+2]-t[j-2]))
          vy <- (d$y[j+2]*scales[2]+d$y[j+1]*scales[2]-d$y[j-1]*scales[2]-d$y[j-2]*scales[2])/(1.5*(t[j+2]-t[j-2]))
          speed[j] <- sqrt(vx^2+vy^2)
        }
      } else if(e$model=='IHMM') {
        speed <- sqrt((c(NA,diff(d$x*scales[1]))/dt)^2+(c(NA,diff(d$y*scales[2]))/dt)^2)
      } else {
        speed <- sqrt((c(NA,diff(d$x))*scales[1])^2+(c(NA,diff(d$y))*scales[2])^2)/dt
      }
      # Smoothed coordinates retain historical whole-trial semantics. Mask every
      # speed whose complete dependency footprint crosses a frozen blocked edge.
      footprint <- lapply(seq_len(n),function(j) {
        if(n<5 || smooth=='raw' || (smooth=='sg'&&n<23))return(j)
        if(smooth %in% c('mean','median'))return(max(1,j-1):min(n,j+1))
        if(j<=11)return(1:23)
        if(j>n-11)return((n-22):n)
        (j-11):(j+11)
      })
      unsafe <- logical(n)
      for(j in seq_len(n)) {
        dep <- if(e$model=='EK03') {if(j<3||j>n-2)integer() else c(j-2,j-1,j+1,j+2)} else {if(j==1)integer() else c(j-1,j)}
        indices <- unlist(footprint[dep],use.names=FALSE)
        if(length(indices))unsafe[j] <- length(unique(g$component_id[min(indices):max(indices)]))>1
      }
      assign(ck,list(speed=speed,unsafe=unsafe),envir=cache)
    }
    z <- get(ck,envir=cache,inherits=FALSE);v <- z$speed[inside]
    finite <- is.finite(v);blocked <- sum(z$unsafe[inside])
    reason <- if(blocked>0)'blocked_derivative_or_smoothing_support' else if(!any(finite))'no_finite_historical_speed' else 'defined'
    result[[length(result)+1]] <- data.frame(event_id=e$event_id,model=e$model,representation=e$representation,
      reference_definition=e$reference_definition,sub=e$sub,Trial_Id=e$Trial_Id,velocity_rule='historical_preprocessed_detector_compatible_supported',
      average_velocity_deg_s=if(reason=='defined')mean(v[finite]) else NA_real_,peak_velocity_deg_s=if(reason=='defined')max(v[finite]) else NA_real_,
      n_derivatives=length(v),n_finite_derivatives=sum(finite),blocked_derivatives=blocked,reason=reason)
  }
  velocity <- do.call(rbind,result)
  write.csv(velocity,file.path(out,'saccade_velocity_events.csv'),row.names=FALSE,na='')
  summary <- velocity %>% group_by(model,representation,reference_definition,velocity_rule) %>%
    summarise(events=n(),defined_events=sum(reason=='defined'),undefined_events=sum(reason!='defined'),
      mean_event_average_velocity_deg_s=mean(average_velocity_deg_s,na.rm=TRUE),SD_event_average_velocity_deg_s=sd(average_velocity_deg_s,na.rm=TRUE),
      mean_event_peak_velocity_deg_s=mean(peak_velocity_deg_s,na.rm=TRUE),SD_event_peak_velocity_deg_s=sd(peak_velocity_deg_s,na.rm=TRUE),.groups='drop')
  write.csv(summary,file.path(out,'saccade_velocity_summary.csv'),row.names=FALSE,na='')
  reasons <- velocity %>% count(model,representation,reference_definition,velocity_rule,reason,name='events')
  write.csv(reasons,file.path(out,'saccade_velocity_missingness.csv'),row.names=FALSE)
  velocity
}
