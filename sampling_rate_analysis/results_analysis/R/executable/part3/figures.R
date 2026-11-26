part3_figures <- function(context,transport,velocity,out) {
  primary <- c('IVT','IDT','EK03','IHMM','CNN_raw')
  cnn <- c('CNN_raw','CNN_mean','CNN_median','CNN_sg_p3_n7','CNN_sg_p5_n23')
  label <- c(IVT='I-VT',IDT='I-DT',EK03='EK03',IHMM='I-HMM',CNN_raw='CNN Raw',CNN_mean='CNN Mean3',CNN_median='CNN Median3',CNN_sg_p3_n7='CNN SG3/7',CNN_sg_p5_n23='CNN SG5/23')
  definitions <- list(fixation_duration=list(event='fixation',metric='native_duration_ms',label='log10(1 + duration / ms)',kind='density'),
    saccade_duration=list(event='saccade',metric='native_duration_ms',label='log10(1 + duration / ms)',kind='density'),
    fixation_counts=list(event='fixation',metric='N',label='Events per trial',kind='PMF'),
    saccade_counts=list(event='saccade',metric='N',label='Events per trial',kind='PMF'),
    saccade_average_velocity=list(event='saccade',metric='average_velocity_deg_s',label='log10(1 + average speed / deg s-1)',kind='density'),
    saccade_peak_velocity=list(event='saccade',metric='peak_velocity_deg_s',label='log10(1 + peak speed / deg s-1)',kind='density'))
  observations <- list();curves <- list();captions <- list()
  key <- function(sub,trial)paste(sub,trial,sep='/')
  ledger <- transport$ledger
  velocity <- velocity[velocity$velocity_rule=='harmonised_raw_within_event',]
  for(bundle in c('primary','CNN_sensitivity'))for(name in names(definitions)) {
    definition <- definitions[[name]];models <- if(bundle=='primary')primary else cnn
    figure <- paste(bundle,name,sep='_');panels <- list()
    availability <- grepl('velocity',name) || definition$kind=='PMF'
    policy <- if(availability)'count_only_common_available' else 'primary_five_common_complete'
    id <- paste('historical','native',definition$event,policy,'IVT','original',sep=':')
    use <- ledger[ledger$comparison_id==id & ledger$included,]
    trial_ids <- key(use$sub,use$Trial_Id)
    for(condition in c('original','rate30'))for(model in models) {
      panel <- paste(model,condition,sep='_');series <- list()
      for(target in c('GT',model)) {
        if(definition$kind=='PMF') {
          q <- context$summary[context$summary$model==target & context$summary$representation==condition & context$summary$event==definition$event &
            context$summary$reference_definition==if(target=='GT')'historical' else 'native',]
          q <- q[q$duration_variant=='native' & key(q$sub,q$Trial_Id) %in% trial_ids,]
          values <- q$N;ids <- key(q$sub,q$Trial_Id)
        } else {
          q <- context$events[context$events$model==target & context$events$representation==condition & context$events$event==definition$event &
            context$events$reference_definition==if(target=='GT')'historical' else 'native',]
          q <- q[key(q$sub,q$Trial_Id) %in% trial_ids,]
          ids <- q$event_id
          if(grepl('velocity',name))values <- velocity[[definition$metric]][match(ids,velocity$event_id)] else values <- q$native_duration_ms
        }
        good <- is.finite(values)&values>=0
        observations[[length(observations)+1]] <- data.frame(figure=figure,panel=panel,series=target,metric=definition$metric,
          observation_id=ids,value=values,included_in_curve=good,support_policy=policy,exposure_trials=length(trial_ids))
        x <- values[good]
        if(definition$kind=='PMF') {
          tab <- table(factor(x,levels=0:max(x)));xx <- as.numeric(names(tab));yy <- as.numeric(tab)/length(x)
        } else {
          d <- density(log10(1+x),n=512,bw='nrd0');xx <- d$x;yy <- d$y
        }
        points <- data.frame(figure=figure,panel=panel,series=target,point=seq_along(xx),x=xx,y=yy,
          included_observations=sum(good),all_observations=length(values),exposure_trials=length(trial_ids),kind=definition$kind)
        curves[[length(curves)+1]] <- points
        series[[target]] <- points
      }
      panels[[panel]] <- list(model=model,condition=condition,series=series)
    }
    all_points <- do.call(rbind,lapply(panels,function(p)do.call(rbind,p$series)))
    xrange <- range(all_points$x,finite=TRUE)
    caption <- paste(if(bundle=='primary')'Fixed development-selected classical configurations and provisional CNN Raw.' else 'Five fixed held-out CNN branches; no outcome-based branch reselection.',
      if(grepl('velocity',name))'Same common successful-call trial exposure for all primary families/both rates; finite event velocities only when defined.' else if(definition$kind=='PMF')'Same common successful-call trial exposure for all primary families/both rates; complete trial counts including zeros.' else 'Same common-complete primary trial exposure across families/both rates; event-pooled duration densities.',
      'Grey: manual sampled reference; blue: model. Labels show finite/all observations. Native boundaries retained.',
      if(definition$kind=='density')'Density is in the labelled log10(1+x) coordinate; no finite-range trimming.' else 'Empirical discrete probability mass; includes successful zero-event trials.')
    caption <- paste(caption,'Reference retains literal legacy NA-run grouping; Original CNN has wider native input exposure.')
    captions[[figure]] <- caption
    for(extension in c('pdf','png')) {
      path <- file.path(out,'figures',paste0(figure,'.',extension))
      if(extension=='pdf')pdf(path,width=15,height=7.6,useDingbats=FALSE) else png(path,width=15,height=7.6,units='in',res=300,type='cairo')
      par(mfrow=c(2,5),mar=c(4,4.6,3.6,.8),oma=c(6,0,1,.4),cex.axis=.8,cex.lab=.8)
      for(p in panels) {
        ymax <- max(unlist(lapply(p$series,function(s)s$y)))*1.13
        plot(NA,xlim=xrange,ylim=c(0,ymax),xlab=definition$label,ylab=if(definition$kind=='PMF')'Probability mass' else 'Density',bty='l')
        for(j in seq_along(p$series)) {
          q <- p$series[[j]];colour <- c('#656565','#2567a5')[j]
          if(definition$kind=='PMF') {lines(q$x,q$y,type='h',col=adjustcolor(colour,.65));points(q$x,q$y,pch=c(1,16)[j],cex=.45,col=colour)} else lines(q$x,q$y,col=colour,lwd=1.5,lty=j)
        }
        mtext(paste(label[p$model],if(p$condition=='original')'Original' else '30 Hz'),3,2,cex=.8,font=2)
        nlabel <- vapply(p$series,function(s)paste0(s$included_observations[1],'/',s$all_observations[1]),character(1))
        mtext(paste('GT',nlabel[1],'; model',nlabel[2]),3,.7,cex=.6)
      }
      mtext(paste(strwrap(caption,width=190),collapse='\n'),side=1,outer=TRUE,line=4,adj=0,cex=.65)
      dev.off()
    }
  }
  write.csv(do.call(rbind,observations),file.path(out,'figure_source_observations.csv'),row.names=FALSE,na='')
  write.csv(do.call(rbind,curves),file.path(out,'figure_source_curves.csv'),row.names=FALSE,na='')
  jsonlite::write_json(captions,file.path(out,'figures/captions.json'),auto_unbox=TRUE,pretty=TRUE)
}
