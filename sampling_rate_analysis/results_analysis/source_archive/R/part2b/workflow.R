part2b_verify_inputs <- function(root) {
  protocol <- jsonlite::fromJSON(file.path(root, 'config/part2b/protocol.json'))
  path <- file.path(root, 'config/part2b/input_manifest.csv')
  stopifnot(file_hash(path) == protocol$input_manifest_sha256)
  registry <- read.csv(path, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(registry))) stopifnot(file_hash(registry$path[i]) == registry$sha256[i])
  list(protocol = protocol, registry = registry, paths = setNames(registry$path, registry$id))
}

part2b_select <- function(context) {
  selected <- list(); audit <- list(); supports <- list()
  for (condition in c('original', 'rate30')) for (method in c('IVT','IDT','EK03','IHMM')) {
    pairs <- joint_pairs(context$definitions, method)
    calls <- context$calls[[condition]]
    availability <- sapply(seq_len(nrow(pairs)), function(i) calls[[pairs$fix_id[i]]]$ok & calls[[pairs$sacc_id[i]]]$ok)
    n <- colSums(availability * context$weight)
    top <- which(n == max(n))
    aligned <- apply(availability[,top,drop=FALSE], 1, all)
    rows <- sum(context$weight[aligned])
    identity <- joint_hash(sort(context$mask$source_row_id[aligned[context$index]]))
    fm <- sm <- objective <- rep(NA_real_, nrow(pairs))
    for (i in top) {
      fm[i] <- joint_mcc(colSums(calls[[pairs$fix_id[i]]]$counts[aligned,,drop=FALSE]))
      sm[i] <- joint_mcc(colSums(calls[[pairs$sacc_id[i]]]$counts[aligned,,drop=FALSE]))
      objective[i] <- (fm[i] + sm[i])/2
    }
    eligible <- top[is.finite(objective[top])]
    stopifnot(rows > 0, max(n) > 0, length(eligible) > 0)
    winner <- eligible[which(objective[eligible] >= max(objective[eligible]) - 1e-12)[1]]
    ranked <- eligible[order(-objective[eligible], eligible)]
    rank <- match(seq_len(nrow(pairs)), ranked)
    for (i in seq_len(nrow(pairs))) {
      audit[[length(audit)+1]] <- data.frame(algorithm=method,representation=condition,candidate_order=i,pairs[i,],
        jointly_available_rows=n[i],maximum_available_rows=max(n),joint_available_trials=sum(availability[,i]),
        unavailable_trial_indices=paste(which(!availability[,i]),collapse=','),maximum_availability=i %in% top,
        aligned_rows=rows,aligned_row_sha256=identity,fixation_MCC=fm[i],saccade_MCC=sm[i],J=objective[i],
        objective_rank=rank[i],selected=i==winner,tie_with_best=i %in% eligible && objective[i]>=max(objective[eligible])-1e-12,
        disposition=if(i==winner)'selected' else if(!i %in% top)'excluded_lower_availability' else if(!is.finite(objective[i]))'excluded_undefined_MCC' else 'eligible_not_selected')
    }
    selected[[length(selected)+1]] <- data.frame(algorithm=method,representation=condition,pairs[winner,],
      selection_policy='full_development_reliability_first_joint_MCC',configuration_status='development_selected',
      development_participants=length(context$participants),development_evaluation_rows=rows,
      maximum_joint_available_rows=max(n),development_row_sha256=identity,
      development_fixation_MCC=fm[winner],development_saccade_MCC=sm[winner],development_J=objective[winner])
    supports[[length(supports)+1]] <- data.frame(algorithm=method,representation=condition,
      participants=paste(context$participants,collapse=','),aligned_trial_indices=paste(which(aligned),collapse=','),
      aligned_rows=rows,row_id_sha256=identity,maximum_availability_candidates=length(top))
  }
  list(selected=do.call(rbind,selected),audit=do.call(rbind,audit),support=do.call(rbind,supports))
}

part2b_stability <- function(final, lopo) {
  result <- list()
  distribution <- function(x) {
    t <- table(x, useNA='ifany');paste(paste(names(t),as.integer(t),sep=':'),collapse=';')
  }
  for(i in seq_len(nrow(final))) {
    f <- final[i,];q <- lopo[lopo$method==f$algorithm & lopo$condition==f$representation,]
    stopifnot(nrow(q)==20,all(q$outcome=='selected'))
    identities <- paste(q$fix_id,q$sacc_id,sep='|');freq <- table(identities)
    modes <- names(freq)[freq==max(freq)]
    selected_id <- paste(f$fix_id,f$sacc_id,sep='|')
    exact <- q$fix_id==f$fix_id & q$sacc_id==f$sacc_id
    threshold_stat <- function(x,fun) if(all(is.na(x))) NA_real_ else fun(x,na.rm=TRUE)
    result[[i]] <- data.frame(f,outer_folds=20,folds_same_preprocessing=sum(q$preprocessing==f$preprocessing),
      folds_exact_configuration=sum(exact),preprocessing_frequencies=distribution(q$preprocessing),
      fixation_threshold_frequencies=distribution(q$fix_threshold),saccade_threshold_frequencies=distribution(q$sacc_threshold),
      fixation_threshold_unique=length(unique(q$fix_threshold[!is.na(q$fix_threshold)])),
      saccade_threshold_unique=length(unique(q$sacc_threshold[!is.na(q$sacc_threshold)])),
      fixation_threshold_min=threshold_stat(q$fix_threshold,min),fixation_threshold_max=threshold_stat(q$fix_threshold,max),
      fixation_threshold_sd=threshold_stat(q$fix_threshold,sd),saccade_threshold_min=threshold_stat(q$sacc_threshold,min),
      saccade_threshold_max=threshold_stat(q$sacc_threshold,max),saccade_threshold_sd=threshold_stat(q$sacc_threshold,sd),
      modal_configuration_ids=paste(sort(modes),collapse=';'),modal_configuration_frequency=max(freq),
      final_agrees_with_any_mode=selected_id %in% modes,mode_tied=length(modes)>1)
  }
  do.call(rbind,result)
}

run_part2b <- function(root,workspace) {
  input <- part2b_verify_inputs(root)
  destination <- file.path(root,'output/part2b')
  if(file.exists(file.path(destination,'output_manifest.json'))) {
    manifest <- jsonlite::fromJSON(file.path(destination,'output_manifest.json'))
    stopifnot(isTRUE(manifest$passed))
    for(name in names(manifest$outputs)) stopifnot(file_hash(file.path(destination,name))==manifest$outputs[[name]])
    for(name in names(manifest$implementation)) stopifnot(file_hash(file.path(root,name))==manifest$implementation[[name]])
    cat('Part 2B frozen inputs, earlier bundles and completed outputs verified; reused.\n')
    return(invisible(NULL))
  }
  dir.create(destination,recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(destination,'validation'),showWarnings=FALSE)
  required <- c('configuration_selection_audit','development_support','configuration_stability',
                'cnn_five_branch_performance','cnn_branch_ranking','final_fixed_configurations','performance_interpretation')
  if (!all(file.exists(file.path(destination,paste0(required,'.csv'))))) {
  paths <- part2_inputs(root)$paths
  context <- part2_context(paths)
  write.csv(context$audit,file.path(destination,'validation/input_sufficiency.csv'),row.names=FALSE)
  cat('Full candidate ledgers and eligible-row identities verified before development selection.\n')
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
  capture.output(sessionInfo(),file=file.path(destination,'validation/sessionInfo.txt'))
  } else cat('Reusing existing numeric products; complete independent verification remains mandatory.\n')
  python <- file.path(workspace,'sampling_rate_analysis/extension_c/c3_cnn_retraining/py311/python.exe')
  log <- system2(python,c('-B',shQuote(file.path(root,'validation/part2b/audit_and_verify.py')),shQuote(root)),stdout=TRUE,stderr=TRUE)
  writeLines(log,file.path(destination,'validation/independent.log'))
  if(!is.null(attr(log,'status')) && attr(log,'status')!=0) stop('Part 2B independent gate failed; partial products retained')
  invisible(part2b_verify_inputs(root))
  files <- list.files(destination,recursive=TRUE,full.names=TRUE)
  files <- files[!dir.exists(files) & basename(files)!='output_manifest.json']
  relative <- substring(files,nchar(destination)+2)
  implementation <- c(file.path(root,'run_results.R'),
    list.files(file.path(root,'R/part2b'),full.names=TRUE),
    list.files(file.path(root,'validation/part2b'),full.names=TRUE),
    list.files(file.path(root,'config/part2b'),full.names=TRUE))
  names_implementation <- substring(implementation,nchar(root)+2)
  manifest <- list(passed=TRUE,stage='part2b',
    outputs=as.list(setNames(vapply(files,file_hash,character(1)),relative)),
    implementation=as.list(setNames(vapply(implementation,file_hash,character(1)),names_implementation)),
    input_manifest_sha256=file_hash(file.path(root,'config/part2b/input_manifest.csv')))
  jsonlite::write_json(manifest,file.path(destination,'output_manifest.json'),auto_unbox=TRUE,pretty=TRUE)
  cat('Part 2B independent numerical and event-input validation PASS.\n')
}
