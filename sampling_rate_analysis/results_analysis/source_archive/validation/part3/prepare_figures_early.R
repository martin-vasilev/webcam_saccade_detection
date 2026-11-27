# Produce authorised figures independently of the still-running exact OT costs.
# The support is a deterministic function of already verified trial summaries.
a<-commandArgs(FALSE);p<-sub('^--file=','',a[grepl('^--file=',a)]);root<-normalizePath(file.path(dirname(p),'../..'),winslash='/');w<-normalizePath(file.path(root,'../..'),winslash='/')
.libPaths(c(file.path(w,'classical_detection_reproduction/R_library'),file.path(w,'classical_evaluation_reproduction/R_library'),.libPaths()))
source(file.path(root,'R/inputs.R'));source(file.path(root,'R/part3/figures.R'))
out<-file.path(root,'output/part3');stage<-file.path(out,'validation/figure_staging')
dir.create(file.path(stage,'figures'),recursive=TRUE,showWarnings=FALSE)
cache<-jsonlite::fromJSON(file.path(out,'validation/pretransport_cache.json'))
stopifnot(isTRUE(cache$passed))
for(n in names(cache$files))stopifnot(file_hash(n)==cache$files[[n]])
paths<-read.csv(file.path(root,'config/part3/input_manifest.csv'));trials<-read.csv(paths$path[paths$id=='trials'])
s<-read.csv(file.path(out,'event_trial_summary.csv'));key<-function(d)paste(d$sub,d$Trial_Id,sep='/');tk<-key(trials)
ledger<-list()
for(event in c('fixation','saccade')) {
 q<-s[s$duration_variant=='native' & s$event==event,]
 get<-function(model,condition){z<-q[q$model==model & q$representation==condition & q$reference_definition==if(model=='GT')'historical' else 'native',];z[match(tk,key(z)),]}
 gt<-get('GT','original');common<-gt$complete_N_Mean_SD;available<-gt$available
 for(model in c('IVT','IDT','EK03','IHMM','CNN_raw'))for(condition in c('original','rate30')) {
  z<-get(model,condition);stopifnot(nrow(z)==792)
  common<-common & z$complete_N_Mean_SD;available<-available & z$available
 }
 for(policy in c('primary_five_common_complete','count_only_common_available')) {
  include<-if(policy=='primary_five_common_complete')common else available
  ledger[[length(ledger)+1]]<-data.frame(comparison_id=paste('historical','native',event,policy,'IVT','original',sep=':'),trials,
    support_policy=policy,included=include,exclusion_reason=ifelse(include,'included','excluded_by_frozen_common_support'))
 }
}
ledger<-do.call(rbind,ledger)
write.csv(ledger,file.path(stage,'figure_support_source.csv'),row.names=FALSE)
context<-list(events=read.csv(file.path(out,'derived/native_events.csv')),summary=s)
part3_figures(context,list(ledger=ledger),read.csv(file.path(out,'saccade_velocity_events.csv')),stage)
cat('Twelve figure pairs staged from frozen-rule support and verified scientific tables. No transport cost used.\n')
