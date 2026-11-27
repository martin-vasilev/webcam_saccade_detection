a<-commandArgs(FALSE);p<-sub('^--file=','',a[grepl('^--file=',a)]);root<-normalizePath(file.path(dirname(p),'../..'),winslash='/');w<-normalizePath(file.path(root,'../..'),winslash='/')
.libPaths(c(file.path(w,'classical_evaluation_reproduction/R_library'),.libPaths()))
d<-read.csv(file.path(root,'output/part3/event_trial_summary.csv'))
d<-d[d$event=='fixation' & d$representation=='original' & d$duration_variant=='native',]
g<-d[d$model=='GT'&d$reference_definition=='historical',];z<-d[d$model=='IVT',]
key<-function(d)paste(d$sub,d$Trial_Id,sep='/');z<-z[match(key(g),key(z)),]
keep<-complete.cases(g[c('N','Mean','SD')])&complete.cases(z[c('N','Mean','SD')])
x<-as.matrix(g[keep,c('N','Mean','SD')]);y<-as.matrix(z[keep,c('N','Mean','SD')]);s<-apply(x,2,sd);s[s==0]<-1;c<-colMeans(x)
x<-scale(x,center=c,scale=s);y<-scale(y,center=c,scale=s)
cat('First real compatibility comparison, n=',nrow(x),as.character(Sys.time()),'\n');flush.console()
for(columns in list(1:3,1L,2L,3L)) {
 cat('Starting columns',paste(columns,collapse=','),as.character(Sys.time()),'\n');flush.console()
 t<-system.time(result<-T4transport::wasserstein(x[,columns,drop=FALSE],y[,columns,drop=FALSE],p=1)$distance)
 cat('Distance',result,'elapsed',t[['elapsed']],'seconds\n');flush.console()
}
