calculate_part2b <- function(context,root,destination) {
  selection <- part2b_select(context)
  save_csv <- function(data,name) write.csv(data,file.path(destination,paste0(name,'.csv')),row.names=FALSE,na='')
  save_csv(selection$audit,'configuration_selection_audit')
  save_csv(selection$support,'development_support')
  lopo <- read.csv(file.path(root,'output/part2/tables/selected_configurations.csv'))
  save_csv(part2b_stability(selection$selected,lopo),'configuration_stability')
  cnn <- read.csv(file.path(root,'output/part2/tables/cnn_five_branch_performance.csv'))
  save_csv(cnn,'cnn_five_branch_performance')
  cnn_ranking <- list()
  for(condition in c('original','rate30')) {
    for(branch in c('raw','mean','median','sg_p3_n7','sg_p5_n23')) {
      q <- cnn[cnn$representation==condition & cnn$branch==branch,]
      stopifnot(nrow(q)==2,length(unique(q$evaluation_rows))==1)
      cnn_ranking[[length(cnn_ranking)+1]] <- data.frame(representation=condition,branch=branch,
        fixation_MCC=q$MCC[q$event=='fixation'],saccade_MCC=q$MCC[q$event=='saccade'],
        J=mean(q$MCC),evaluation_rows=q$evaluation_rows[1],interpretation='post_hoc_descriptive_branch_ranking')
    }
  }
  ranking <- do.call(rbind,cnn_ranking)
  ranking$descriptive_rank <- ave(-ranking$J,ranking$representation,FUN=function(x)rank(x,ties.method='min'))
  ranking$observed_highest <- ranking$descriptive_rank==1
  save_csv(ranking,'cnn_branch_ranking')
  final <- selection$selected
  for(condition in c('original','rate30')) {
    q <- as.data.frame(as.list(setNames(rep(NA,ncol(final)),names(final))))
    q$algorithm <- 'CNN-BLSTM';q$representation <- condition;q$preprocessing <- 'Raw'
    q$fix_id <- 'CNN_raw';q$sacc_id <- 'CNN_raw'
    q$selection_policy <- 'provisional_common_Raw_minimal_transformation_not_optimised'
    q$configuration_status <- 'provisional_requires_scientific_confirmation'
    final <- rbind(final,q)
  }
  save_csv(final,'final_fixed_configurations')
  estimates <- list()
  for(i in seq_len(nrow(selection$selected))) {
    f <- selection$selected[i,];support <- selection$support[i,]
    keep <- seq_len(nrow(context$trials)) %in% as.integer(strsplit(support$aligned_trial_indices,',',fixed=TRUE)[[1]])
    for(event in c('fixation','saccade')) {
      id <- if(event=='fixation')f$fix_id else f$sacc_id
      co <- colSums(context$calls[[f$representation]][[id]]$counts[keep,,drop=FALSE])
      estimates[[length(estimates)+1]] <- data.frame(algorithm=f$algorithm,representation=f$representation,event=event,
        estimate_type='development_selected_fixed_in_sample',selection_policy=f$selection_policy,configuration_id=id,
        evaluation_support='aligned_maximum_availability_development',evaluation_rows=sum(context$weight[keep]),
        row_id_sha256=support$row_id_sha256,TP=co[1],TN=co[2],FP=co[3],FN=co[4],as.list(classification_metrics(co)))
    }
  }
  dev <- do.call(rbind,estimates)
  old <- read.csv(file.path(root,'output/part2/tables/heldout_performance.csv'))
  old$estimate_type <- ifelse(old$selection_policy=='historical_fixed','historical_fixed_benchmark','outer_LOPO_policy_heldout_not_final_fixed_configuration')
  old$configuration_id <- 'fold-specific or historical; see immutable Phase 2A choices'
  cnn$estimate_type <- 'fixed_CNN_branch_outer_participant_heldout_no_branch_selector';cnn$configuration_id<-paste0('CNN_',cnn$branch)
  cnn$row_id_sha256 <- joint_hash(sort(context$mask$source_row_id))
  columns <- names(dev)
  save_csv(rbind(dev,old[columns],cnn[columns]),'performance_interpretation')

}
