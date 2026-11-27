file_hash <- function(path) {
  # Windows file.access() can reject sandbox ACL metadata even when the file is readable.
  # Hash the bytes actually read and let a genuine read failure propagate.
  connection <- file(path, open = 'rb')
  on.exit(close(connection))
  bytes <- readBin(connection, what = 'raw', n = file.info(path)$size)
  digest::digest(bytes, algo = 'sha256', serialize = FALSE)
}

verify_inputs <- function(root, config) {
  manifest_path <- file.path(root, 'config/input_manifest.csv')
  stopifnot(file_hash(manifest_path) == config$input_manifest_sha256)
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(nrow(manifest))) {
    entry <- manifest[i, ]
    if (!file.exists(entry$path) || file_hash(entry$path) != entry$sha256) stop('Input identity mismatch: ', entry$id)
    if (nzchar(entry$archive_sha256) && entry$sha256 != entry$archive_sha256) stop('Archive identity mismatch: ', entry$id)
  }
  paths <- setNames(manifest$path, manifest$id)
  for (key in c('b_gate', 'h_gate', 'o_gate', 'n_gate', 'downstream_gate')) {
    gate <- jsonlite::fromJSON(paths[[key]])
    stopifnot(isTRUE(gate$passed))
  }
  paths
}

verify_completed <- function(destination, root) {
  manifest_file <- file.path(destination, 'output_manifest.json')
  if (!file.exists(manifest_file)) stop('Existing outputs lack final completion manifest; inspect before resuming')
  manifest <- jsonlite::fromJSON(manifest_file)
  stopifnot(isTRUE(manifest$passed))
  for (name in names(manifest$outputs)) stopifnot(file_hash(file.path(destination, name)) == manifest$outputs[[name]])
  for (name in names(manifest$implementation)) stopifnot(file_hash(file.path(root, name)) == manifest$implementation[[name]])
}
