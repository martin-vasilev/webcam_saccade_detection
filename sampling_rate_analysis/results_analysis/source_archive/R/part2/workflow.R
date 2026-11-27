run_part2 <- function(root, workspace) {
  inputs <- part2_inputs(root)
  destination <- file.path(root, 'output/part2')
  if (dir.exists(destination)) {
    manifest <- jsonlite::fromJSON(file.path(destination, 'output_manifest.json'))
    stopifnot(isTRUE(manifest$passed))
    for (name in names(manifest$outputs)) stopifnot(file_hash(file.path(destination, name)) == manifest$outputs[[name]])
    for (name in names(manifest$implementation)) stopifnot(file_hash(file.path(root, name)) == manifest$implementation[[name]])
    cat('Part 2 completed outputs and frozen inputs verified; reused.\n')
    return(invisible(NULL))
  }
  pending_runs <- list.dirs(file.path(root, 'output'), recursive = FALSE, full.names = TRUE)
  complete_tables <- vapply(pending_runs, function(path) {
    grepl('^part2_pending_', basename(path)) &&
      all(file.exists(file.path(path, c('tables/candidate_scores.csv', 'tables/heldout_performance.csv',
                                       'tables/cnn_five_branch_performance.csv', 'validation/plot_ledger.csv'))))
  }, logical(1))
  if (any(complete_tables)) {
    pending <- tail(sort(pending_runs[complete_tables]), 1)
    cat('Reusing complete pending tables; independent verification is required before commit.\n')
    part2_validate_and_commit(root, workspace, pending, destination)
    return(invisible(NULL))
  }
  pending <- file.path(root, 'output', paste0('part2_pending_', format(Sys.time(), '%Y%m%d_%H%M%S'), '_', Sys.getpid()))
  for (name in c('tables', 'figures', 'validation')) dir.create(file.path(pending, name), recursive = TRUE)
  context <- part2_context(inputs$paths)
  write.csv(context$audit, file.path(pending, 'validation/input_sufficiency.csv'), row.names = FALSE)
  cat('Complete candidate/trial counts and aligned reference support verified before selection.\n')
  selection <- select_joint(context)
  stopifnot(all(selection$selections$outcome == 'selected'))
  choices <- policy_calls(context, selection$selections, inputs$paths)
  results <- evaluate_policies(context, choices)
  cnn <- evaluate_cnn(inputs$paths)
  tables <- list(selected_configurations = selection$selections, training_support = selection$training,
    candidate_scores = selection$candidates, policy_configuration_choices = choices,
    heldout_trial_counts = results$trials, heldout_performance = results$pooled,
    heldout_participant_performance = results$participants, cnn_five_branch_performance = cnn,
    policy_comparison_shared_support = results$pooled[results$pooled$evaluation_support == 'shared_policy_paired_joint', ],
    existing_B2_original_support = part2_read(inputs$paths, 'b_old_summary'),
    existing_C1_original_support = part2_read(inputs$paths, 'h_old_summary'))
  for (name in names(tables)) write.csv(tables[[name]], file.path(pending, 'tables', paste0(name, '.csv')), row.names = FALSE, na = '')
  part2_figures(results$pooled, cnn, pending)
  capture.output(sessionInfo(), file = file.path(pending, 'validation/sessionInfo.txt'))
  part2_validate_and_commit(root, workspace, pending, destination)
}

part2_validate_and_commit <- function(root, workspace, pending, destination) {
  python <- file.path(workspace, 'sampling_rate_analysis/extension_c/c3_cnn_retraining/py311/python.exe')
  log <- system2(python, c('-B', shQuote(file.path(root, 'validation/part2/verify.py')), shQuote(root), shQuote(pending)), stdout = TRUE, stderr = TRUE)
  writeLines(log, file.path(pending, 'validation/independent.log'))
  if (!is.null(attr(log, 'status')) && attr(log, 'status') != 0) stop('Independent Part 2 verification failed; pending files retained')
  invisible(part2_inputs(root))
  if (!file.rename(pending, destination)) stop('Could not commit new Part 2 directory')
  cat('Part 2 numeric validation passed; visual completion gate pending.\n')
}
