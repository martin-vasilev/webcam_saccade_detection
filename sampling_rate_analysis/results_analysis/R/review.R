# Presentation only: filter/relabel saved aggregates, never select scientific configurations.
put_table <- function(root,name,x) {
  folder <- file.path(root,'output/review/tables');dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  write.csv(x,file.path(folder,paste0(name,'.csv')),row.names=FALSE,na='')
  display <- x
  for(j in seq_along(display))if(is.numeric(display[[j]]))display[[j]]<-format(round(display[[j]],3),trim=TRUE,na.encode=FALSE)
  escape <- function(x)gsub('|',' / ',ifelse(is.na(x),'',as.character(x)),fixed=TRUE)
  lines <- c(paste0('# Provisional review - ',gsub('_',' ',name)), '',
    'Full-precision values: matching CSV. Definitions and support limitations: METHODS.md.', '',
    paste('|',paste(names(display),collapse=' | '),'|'),paste('|',paste(rep('---',ncol(display)),collapse=' | '),'|'),
    apply(display,1,function(r)paste('|',paste(escape(r),collapse=' | '),'|')))
  writeLines(lines,file.path(folder,paste0(name,'.md')))
}
save_review_plot <- function(root,name,width,height,draw,caption) {
  folder<-file.path(root,'output/review/figures');dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  for(ext in c('pdf','png')) {
    p<-file.path(folder,paste0(name,'.',ext))
    if(ext=='pdf')pdf(p,width=width,height=height,useDingbats=FALSE) else png(p,width=width,height=height,units='in',res=300,type='cairo')
    draw();dev.off()
  }
  writeLines(caption,file.path(folder,paste0(name,'.caption.txt')))
}
make_review <- function(root,stage) {
  for(d in c('output/review/tables','output/review/figures','output/supplementary','output/provenance','validation'))dir.create(file.path(root,d),recursive=TRUE,showWarnings=FALSE)
  if(stage=='part1') {
    d<-setNames(lapply(c('grid','hmm','cnn','cnn_primary','selected','hmm_selected','common','grid_definitions'),
      function(x)read_frozen(root,stage,paste0(x,'_source.csv'))),c('grid','hmm','cnn','cnn_primary','selected','hmm_selected','common','grid_definitions'))
    source(file.path(root,'R/part1_figures.R'),local=FALSE)
    temp<-file.path(root,'output/supplementary/part1_presentation')
    for(q in c('figures','tables','validation'))dir.create(file.path(temp,q),recursive=TRUE,showWarnings=FALSE)
    draw_figures(d,temp)
    files<-list.files(file.path(temp,'figures'),full.names=TRUE)
    for(f in files)if(!grepl('05_existing',basename(f)))file.copy(f,file.path(root,'output/review/figures',basename(f)),overwrite=TRUE)
    put_table(root,'available_configurations_and_support',read_frozen(root,stage,'configurations_and_support.csv'))
  }
  if(stage=='part2') {
    a<-read_frozen(root,stage,'heldout_performance.csv')
    a<-a[a$selection_policy=='shared_preprocessing_joint' & a$evaluation_support=='own_policy_paired_joint',]
    b<-read_frozen(root,stage,'cnn_five_branch_performance.csv');b<-b[b$branch=='raw',]
    cols<-c('algorithm','representation','event','MCC','F1','precision','recall','evaluation_rows','evaluation_support')
    x<-rbind(a[,cols],b[,cols]);put_table(root,'sample_level_heldout_policy_and_CNN_Raw',x)
    put_table(root,'sample_level_prediction_availability',rbind(a[,c('algorithm','representation','event','available_trials','failed_trials','evaluation_rows')],b[,c('algorithm','representation','event','available_trials','failed_trials','evaluation_rows')]))
    draw<-function(){
      # Leave room for the rotated MCC label in the first column on raster devices.
      par(mfrow=c(2,5),mar=c(4,4.2,2.7,.7),oma=c(2,0,3,0))
      for(e in c('fixation','saccade'))for(m in unique(x$algorithm)) {
        z<-x[x$algorithm==m & x$event==e,];z<-z[match(c('original','rate30'),z$representation),]
        plot(1:2,z$MCC,pch=16,col=c('#376a99','#c06439'),xlim=c(.7,2.3),ylim=c(-.1,.65),xaxt='n',xlab='',ylab='MCC',main=paste(m,e),cex.main=.8)
        axis(1,1:2,c('Original','30 Hz'),cex.axis=.75);abline(h=0,col='grey80')
        mtext(paste('rows:',paste(z$evaluation_rows,collapse='/')),side=3,line=.1,cex=.55)
      }
      mtext('Provisional review | Held-out policy / fixed CNN Raw branch',outer=TRUE,side=3,line=1,font=2)
    }
    save_review_plot(root,'sample_level_heldout_by_method',13,6.5,draw,c('Classical: training-only joint preprocessing LOPO selection, own-policy paired joint support.',
      'CNN Raw: fixed held-out branch, not selected on held-out MCC. Supports differ across methods; no direct inferential ranking.',
      'Exact denominators, confusion counts and all CNN branches are in data/frozen/part2/. No new intervals are calculated.'))
  }
  if(stage=='part2b') {
    x<-read_frozen(root,stage,'final_fixed_configurations.csv')
    put_table(root,'development_selected_fixed_configurations',x[,c('algorithm','representation','preprocessing','fix_threshold','sacc_threshold','configuration_status','development_evaluation_rows')])
    put_table(root,'fixed_configuration_stability',read_frozen(root,stage,'configuration_stability.csv')[,c('algorithm','representation','outer_folds','folds_same_preprocessing','folds_exact_configuration')])
  }
  if(stage=='part3') {
    w<-read_frozen(root,stage,'wasserstein_harmonised.csv')
    w<-w[w$support_policy=='primary_five_common_complete' & w$reference_definition=='historical' & w$duration_variant=='native',]
    put_table(root,'event_transport_common_complete',w[,c('model','representation','event','n_trials','distance_multi','distance_N','distance_Mean','distance_SD','similarity_multi')])
    a<-read_frozen(root,stage,'event_distribution_summary.csv')
    a<-a[a$duration_variant=='native' & (a$model %in% c('IVT','IDT','EK03','IHMM','CNN_raw') | a$reference_definition=='historical'),]
    put_table(root,'native_events_all_available_trials',a[,c('model','representation','reference_definition','event','available_trials','zero_event_trials','pooled_events','trial_N_mean','trial_Mean_duration_mean_ms','trial_SD_duration_mean_ms','Mean_denominator','SD_denominator')])
    v<-read_frozen(root,stage,'saccade_velocity_summary.csv')
    v<-v[v$velocity_rule=='harmonised_raw_within_event' & (v$model %in% c('IVT','IDT','EK03','IHMM','CNN_raw') | v$reference_definition=='historical'),]
    put_table(root,'saccade_velocity_harmonised',v[,c('model','representation','reference_definition','events','defined_events','undefined_events','mean_event_average_velocity_deg_s','mean_event_peak_velocity_deg_s')])
    curves<-read_frozen(root,stage,'figure_source_curves.csv')
    for(fig in unique(curves$figure)) {
      z<-curves[curves$figure==fig,];panels<-unique(z$panel)
      draw<-local({z<-z;panels<-panels;fig<-fig;function(){
        par(mfrow=c(2,length(panels)/2),mar=c(4.1,3.5,2.4,.6),oma=c(2.2,0,3.3,0),mgp=c(2,.6,0))
        count<-grepl('counts',fig);velocity<-grepl('velocity',fig)
        for(panel in panels){
          a<-z[z$panel==panel,];xr<-range(a$x,finite=TRUE);yr<-range(c(0,a$y),finite=TRUE)
          plot(NA,xlim=xr,ylim=yr,xlab=if(count)'Events / trial' else if(velocity)'log10(1 + speed [deg/s])' else 'log10(1 + duration [ms])',ylab=if(count)'Probability' else 'Density on transformed scale',main=gsub('_',' ',panel),cex.main=.7,cex.lab=.73)
          for(s in unique(a$series)) {b<-a[a$series==s,];lines(b$x,b$y,type=if(count)'h' else 'l',col=if(s=='GT')'#555555' else '#007d87',lty=if(s=='GT')2 else 1,lwd=1.4)}
          mtext(paste('Trials:',paste(unique(a$exposure_trials),collapse='/')),side=3,line=.2,cex=.55)
          legend('topright',legend=unique(a$series),col=ifelse(unique(a$series)=='GT','#555555','#007d87'),lty=ifelse(unique(a$series)=='GT',2,1),bty='n',cex=.56)
        }
        mtext(paste('Provisional review |',gsub('_',' ',fig)),outer=TRUE,side=3,line=1,font=2,cex=.95)
        mtext('Archived curves; panel supports and reference definitions in config/part3_curve_captions.json',outer=TRUE,side=1,line=.6,cex=.65)
      }})
      save_review_plot(root,fig,if(length(panels)>8)14 else 12,7,draw,c('Replotted archived full-precision curves; density/transport not recomputed.',
        'See matching original caption in config/part3_curve_captions.json and figure_source_curves.csv for denominators.'))
      if(grepl('^CNN_sensitivity',fig)){
        for(ext in c('pdf','png','caption.txt'))file.rename(file.path(root,'output/review/figures',paste0(fig,'.',ext)),file.path(root,'output/supplementary',paste0(fig,'.',ext)))
      }
    }
  }
}
