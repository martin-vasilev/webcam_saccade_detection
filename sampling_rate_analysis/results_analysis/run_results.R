args <- commandArgs(trailingOnly = TRUE)
option <- function(key, default) {
  i <- match(paste0('--', key), args)
  if (is.na(i)) default else if (i == length(args)) stop('Missing value for ', key) else args[i + 1]
}
file_arg <- grep('^--file=', commandArgs(), value = TRUE)
root <- normalizePath(dirname(sub('^--file=', '', file_arg[1])), winslash = '/', mustWork = TRUE)
stage <- option('stage', 'all'); mode <- option('mode', 'verify')
stopifnot(stage %in% c('all', 'part1', 'part2', 'part2b', 'part3'))
if (!mode %in% c('verify', 'review','recompute')) stop('Supported modes: verify, review, recompute; see REPRODUCIBILITY.md.')
for (package in c('jsonlite', 'digest')) if (!requireNamespace(package, quietly = TRUE)) stop('Install R package: ', package)
if(mode=='recompute') {
  source(file.path(root,'R/scientific_pipeline.R'))
  scientific_run(root,stage,args,option)
  quit(save='no',status=0)
}
scientific_root<-option('scientific-root','')
if(nzchar(scientific_root)) {
  scientific_root<-normalizePath(scientific_root,winslash='/',mustWork=TRUE)
  required<-if(stage=='all')c('part1','part2','part2b','part3')else stage
  for(s in required) {
    p<-file.path(scientific_root,'validation',paste0(s,'_computation.json'))
    if(!file.exists(p))stop('Missing actual scientific checkpoint: ',s)
    m<-jsonlite::fromJSON(p)
    for(n in names(m$output_hashes)) {
      f<-file.path(scientific_root,n)
      stopifnot(file.exists(f),digest::digest(readBin(f,'raw',n=file.info(f)$size),algo='sha256',serialize=FALSE)==m$output_hashes[[n]])
    }
  }
  python<-option('python',Sys.getenv('PYTHON','python'))
  log<-file.path(scientific_root,'validation/comparison_command.log')
  code<-system2(python,c('-B',shQuote(file.path(root,'python/compare_scientific.py')),shQuote(root),shQuote(scientific_root)),stdout=log,stderr=log)
  if(code!=0)stop('Recomputed scientific comparison failed; see ',log)
  if(mode=='review') {source(file.path(root,'R/scientific_review.R'));review_scientific(root,scientific_root,stage)}
  cat('Scientific outputs present in configured run: frozen numerical comparison PASS. See run_status.json for full/subset execution scope.\n')
  quit(save='no',status=0)
}
source(file.path(root, 'R/release.R'))
source(file.path(root, 'R/review.R'))
source(file.path(root, 'R/scientific/part2/selection.R'))
source(file.path(root, 'R/scientific/part2/evaluation.R'))
stages <- if (stage == 'all') c('part1', 'part2', 'part2b', 'part3') else stage
cat('Webcam eye-event detection - Results review package\nMode:', mode, '(archived science; no reselection or inference)\n\n')
results <- list()
for (s in stages) {
  results[[s]] <- verify_frozen(root, s)
  if (mode == 'review') make_review(root, s)
  cat(sprintf('%-8s Frozen numerical verification PASS%s\n', s, if (mode == 'review') '; review regenerated' else ''))
}
record_environment(root)
jsonlite::write_json(list(passed = all(vapply(results, function(x) x$passed, logical(1))),
  mode = mode, stages = results, scope = 'Archived-result integrity and numerical identities; not full scientific recomputation'),
  file.path(root, 'validation/last_run.json'), auto_unbox = TRUE, pretty = TRUE)
cat('\nReview tables: output/review/tables/\nReview figures: output/review/figures/\nFull-precision aggregate supplements: data/frozen/\nRun validation: PASS (scope above; publication approval remains separate)\n')
