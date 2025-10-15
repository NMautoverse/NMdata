##' Create a data table with columns of unequal length
##'
##' @param ... Vectors to put into data.table
##'
##' @keywords internal


dtFillCols <- function(...){

    dots <- list(...)

    length.max <- max(sapply(dots,length))

    dots <- lapply(dots,function(x)c(x,rep(NA,length.max-length(x))))
    do.call(data.table,dots)
    
}
