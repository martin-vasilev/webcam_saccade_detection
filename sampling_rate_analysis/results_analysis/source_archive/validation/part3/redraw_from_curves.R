# Presentation-only redraw: use the independently checked, serialized curve tables.
a<-commandArgs(FALSE);p<-sub('^--file=','',a[grepl('^--file=',a)])
root<-normalizePath(file.path(dirname(p),'../..'),winslash='/');w<-normalizePath(file.path(root,'../..'),winslash='/')
.libPaths(c(file.path(w,'classical_detection_reproduction/R_library'),file.path(w,'classical_evaluation_reproduction/R_library'),.libPaths()))
source(file.path(root,'R/inputs.R'));out<-file.path(root,'output/part3')
if('--staging' %in% commandArgs(TRUE))out<-file.path(out,'validation/figure_staging')
dir.create(file.path(out,'validation'),recursive=TRUE,showWarnings=FALSE)
curves<-read.csv(file.path(out,'figure_source_curves.csv'));captions<-jsonlite::fromJSON(file.path(out,'figures/captions.json'))
labels<-c(IVT='I-VT',IDT='I-DT',EK03='EK03',IHMM='I-HMM',CNN_raw='CNN Raw',CNN_mean='CNN Mean3',CNN_median='CNN Median3',CNN_sg_p3_n7='CNN SG3/7',CNN_sg_p5_n23='CNN SG5/23')
notice<-'Reference retains literal legacy NA-run grouping; Original CNN has wider native input exposure.'
for(name in unique(curves$figure)) {
 data<-curves[curves$figure==name,];kind<-data$kind[1]
 caption<-captions[[name]];if(!grepl('legacy NA-run grouping',caption,fixed=TRUE))caption<-paste(caption,notice)
 if(grepl('_counts$',name))caption<-sub('Same common successful-call trial exposure for all primary families/both rates; finite event velocities only when defined.',
   'Same common successful-call trial exposure for all primary families/both rates; complete trial counts including zeros.',caption,fixed=TRUE)
 captions[[name]]<-caption
 xlab<-if(grepl('_counts$',name))'Events per trial' else if(grepl('_average_velocity$',name))'log10(1 + average speed / deg s-1)' else if(grepl('_peak_velocity$',name))'log10(1 + peak speed / deg s-1)' else 'log10(1 + duration / ms)'
 for(ext in c('pdf','png')) {
  path<-file.path(out,'figures',paste0(name,'.',ext))
  if(ext=='pdf')pdf(path,width=15,height=7.6,useDingbats=FALSE) else png(path,width=15,height=7.6,units='in',res=300,type='cairo')
  par(mfrow=c(2,5),mar=c(4,4.6,3.6,.8),oma=c(6,0,1,.4),cex.axis=.8,cex.lab=.8)
  for(panel in unique(data$panel)) {
   d<-data[data$panel==panel,];model<-sub('_(original|rate30)$','',panel);condition<-if(grepl('_original$',panel))'Original' else '30 Hz'
   plot(NA,xlim=range(data$x),ylim=c(0,max(d$y)*1.13),xlab=xlab,ylab=if(kind=='PMF')'Probability mass' else 'Density',bty='l')
   series<-unique(d$series);nlabels<-character()
   for(j in seq_along(series)) {
    q<-d[d$series==series[j],];colour<-c('#656565','#2567a5')[j]
    if(kind=='PMF'){lines(q$x,q$y,type='h',col=adjustcolor(colour,.65));points(q$x,q$y,pch=c(1,16)[j],cex=.45,col=colour)} else lines(q$x,q$y,col=colour,lwd=1.5,lty=j)
    nlabels[j]<-paste0(q$included_observations[1],'/',q$all_observations[1])
   }
   mtext(paste(labels[model],condition),3,2,cex=.8,font=2)
   mtext(paste('GT',nlabels[1],'; model',nlabels[2]),3,.7,cex=.6)
  }
  mtext(paste(strwrap(caption,width=190),collapse='\n'),side=1,outer=TRUE,line=4,adj=0,cex=.65)
  dev.off()
 }
}
jsonlite::write_json(captions,file.path(out,'figures/captions.json'),auto_unbox=TRUE,pretty=TRUE)
figure_names<-as.vector(outer(c('primary','CNN_sensitivity'),c('fixation_duration','saccade_duration','fixation_counts','saccade_counts','saccade_average_velocity','saccade_peak_velocity'),paste,sep='_'))
files<-c('figure_source_observations.csv','figure_source_curves.csv','figures/captions.json',paste0('figures/',rep(figure_names,each=2),rep(c('.pdf','.png'),length(figure_names))))
gate<-list(inputs_sha256=jsonlite::fromJSON(file.path(root,'config/part3/event_protocol.json'))$input_manifest_sha256,
 outputs=as.list(setNames(vapply(file.path(out,files),file_hash,character(1)),files)),implementation=file_hash(file.path(root,'R/part3/figures.R')))
jsonlite::write_json(gate,file.path(out,'validation/figures_gate.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Twelve figure pairs redrawn from saved curves; scientific source tables unchanged.\n')
