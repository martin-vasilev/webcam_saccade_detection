# Presentation adapter for newly computed products. Frozen aggregates are not score sources here.
review_scientific <- function(package,run,stage) {
  proof<-file.path(run,'validation/frozen_comparison.json')
  if(!file.exists(proof)||!isTRUE(jsonlite::fromJSON(proof)$passed))stop('Run scientific-output verification before review regeneration.')
  e<-new.env(parent=globalenv());sys.source(file.path(package,'R/review.R'),envir=e)
  for(f in c('inputs.R','selection.R','evaluation.R'))sys.source(file.path(package,'R/executable/part2',f),envir=e)
  e$read_frozen<-function(root,s,file) {
    path<-switch(s,part2=file.path(run,'output/part2/tables',file),part2b=file.path(run,'output/part2b',file),part3=file.path(run,'output/part3',file))
    if(is.null(path)||!file.exists(path))stop('Scientific product unavailable: ',s,'/',file)
    read.csv(path,stringsAsFactors=FALSE)
  }
  stages<-if(stage=='all')c('part1','part2','part2b','part3')else stage
  for(s in stages) {
    if(s=='part1') {
      paths<-unlist(jsonlite::fromJSON(file.path(run,'recomputed_paths.json')))
      d<-read.csv(file.path(run,'derived_counts/part1_landscapes.csv'))
      defs<-read.csv(paths[['b_grid']]);g<-d[d$method!='IHMM',]
      g$threshold<-defs$threshold_value[match(g$configuration_id,defs$configuration_id)]
      h<-d[d$method=='IHMM',];hd<-read.csv(paths[['h_grid']]);h$smoothing<-hd$smoothing[match(h$configuration_id,hd$configuration_id)]
      cnn_table<-file.path(run,'output/part2/tables/cnn_five_branch_performance.csv')
      cn<-if(file.exists(cnn_table))read.csv(cnn_table)else e$evaluate_cnn(paths)
      cn$condition<-cn$representation;cn$scored_rows<-cn$evaluation_rows;cn$method<-'CNN'
      cn$preprocessing<-c(raw='Raw',mean='Mean3',median='Median3',sg_p3_n7='SG3_7',sg_p5_n23='SG5_23')[cn$branch]
      common<-list();threshold<-list(IVT=c(fixation=35,saccade=20),IDT=c(fixation=1,saccade=1.5),EK03=c(fixation=3,saccade=2))
      for(m in names(threshold))for(ev in names(threshold[[m]]))common[[length(common)+1]]<-g[g$method==m & g$event==ev & g$preprocessing=='SG5_23' & g$threshold==threshold[[m]][[ev]],c('method','event','condition','MCC','scored_rows')]
      common<-do.call(rbind,c(common,list(h[h$smoothing=='sg',c('method','event','condition','MCC','scored_rows')],cn[cn$preprocessing=='SG5_23',c('method','event','condition','MCC','scored_rows')])))
      dest<-file.path(run,'output/review');for(q in c('figures','tables','validation'))dir.create(file.path(dest,q),recursive=TRUE,showWarnings=FALSE)
      sys.source(file.path(package,'R/part1_figures.R'),envir=e)
      original_save<-e$save_figure
      e$save_figure<-function(name,width,height,draw,caption,destination) {
        caption<-gsub('Point estimates are imported from the validated downstream table. No new intervals or bootstrap draws are calculated.','Scores recomputed from saved held-out predictions. No intervals or bootstrap draws are recalculated.',caption,fixed=TRUE)
        caption<-gsub('Primary seed schedule 0. HMM states are re-estimated per trial and representation; this is a preprocessing landscape.','Saved primary-seed-0 per-trial HMM fits are rescored; state fitting is not rerun.',caption,fixed=TRUE)
        original_save(name,width,height,draw,caption,destination)
      }
      # Presentation-only exclusion of the old three-rate selected-policy context panel.
      body(e$draw_figures)<-parse(text=gsub('c("common", "selected_context")','c("common")',paste(deparse(body(e$draw_figures)),collapse='\n'),fixed=TRUE))[[1]]
      e$draw_figures(list(grid=g,hmm=h,cnn=cn,common=common),dest)
      caption_file<-file.path(dest,'figures/captions.json')
      caps<-jsonlite::fromJSON(caption_file,simplifyVector=FALSE)
      caps<-lapply(caps,function(x){
        x<-gsub('Point estimates are imported from the validated downstream table. No new intervals or bootstrap draws are calculated.','Scores recomputed from saved held-out predictions. No intervals or bootstrap draws are recalculated.',unlist(x),fixed=TRUE)
        gsub('Primary seed schedule 0. HMM states are re-estimated per trial and representation; this is a preprocessing landscape.','Saved primary-seed-0 per-trial HMM fits are rescored; state fitting is not rerun.',x,fixed=TRUE)
      })
      jsonlite::write_json(caps,caption_file,pretty=TRUE,auto_unbox=TRUE)
      for(n in c('grid','hmm','cnn','common'))write.csv(list(grid=g,hmm=h,cnn=cn,common=common)[[n]],file.path(dest,'tables',paste0(n,'_source.csv')),row.names=FALSE)
      write.csv(defs,file.path(dest,'tables/grid_definitions_source.csv'),row.names=FALSE)
    } else {
      if(s=='part3') {
        checkpoint<-jsonlite::fromJSON(file.path(run,'validation/part3_computation.json'))
        if(checkpoint$details$transport_scope!='full')stop('Part 3 scientific review requires full transport/curve products. The representative run does not substitute cached curves.')
      }
      e$make_review(run,s)
    }
  }
  cat('Review products generated from verified recomputed science at',file.path(run,'output/review'),'\n')
}
