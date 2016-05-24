# source all the R files in fold of the path 
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
       if(trace) cat(nm,":")           
       source(file.path(path, nm),encoding="utf-8", ...)
       if(trace) cat("\n")
    }
 }