args <- commandArgs(FALSE); path <- sub('^--file=','',args[grepl('^--file=',args)])
root <- normalizePath(file.path(dirname(path),'../..'),winslash='/');w <- normalizePath(file.path(root,'../..'),winslash='/')
.libPaths(c(file.path(w,'classical_detection_reproduction/R_library'),file.path(w,'classical_evaluation_reproduction/R_library'),.libPaths()))
library(T4transport)
cat('T4transport',as.character(packageVersion('T4transport')),'\n')
print(T4transport::wasserstein)
for(name in c('compute_pdist2','wasserstein_exact')) {
 f <- get0(name,envir=asNamespace('T4transport')); if(!is.null(f))print(f)
}
cat('Fixtures Euclidean diagonal shift and identity\n')
x <- matrix(c(0,0,10,10,20,20),3,2,byrow=TRUE)
print(T4transport::wasserstein(x,x,p=1)$distance)
print(T4transport::wasserstein(x,x+matrix(rep(c(3,4),3),3,2,byrow=TRUE),p=1)$distance)
cat('Libraries\n');print(.libPaths());print(sessionInfo())
