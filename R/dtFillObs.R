##' Create a data table with input columns of unequal length
##'
##' All columns are filled with NA to max length of all input variables. This is
##' useful when constructing partially completed variables where 1) variables
##' are complete up to their length and 2) the missing values should be set to
##' NA.
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
