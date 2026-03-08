##' A standard-evaluation interface to `data.table::dcast()
##'
##' data.table's dcast() transforms from long to wide format. It lacks a standard-evalutation interface for programming, and dcastSe() is an attempt to fill that gap, still using data.table::dcast() to perform the operation.
##'
##' @param data data set to transpose (widen)
##' @param l left-hand side variables as character vector. Result will
##'     be long/vertical in these variables.
##' @param r left-hand side variables as character vector. Result will
##'     be wide in these variables.
##' @param ... Additional arguments passed to `data.table::dcast()`.
##' @details Only two of l, r, and value.var are needed (see ?data.table::dcast for value.var), and the others will be derived. However, if value.var is a function, both l and r are needed.
##' @import data.table 
##' @importFrom stats as.formula
##' @return a data.table
##' @export

dcastSe <- function(data,l,r,...){

    if(!is.data.table(data)){
        data <- as.data.table(data)
    }

    if(missing(l)) l <- NULL
    if(missing(r)) r <- NULL

    dots <- list(...)
    if("value.var"%in%names(dots) && is.character(dots$value.var)){
        if(is.null(l) && is.null(r)) stop ("At least one of l and r must be provided")
        if(is.null(r)){
            r <- setdiff(names(data),unique(c(l,dots$value.var)))
        }
        if(is.null(l)){
            l <- setdiff(names(data),unique(c(r,dots$value.var)))
        }
    } else {
        if(is.null(l) || is.null(r)) stop ("When value.var is not provided, both l and r must be provided.")
    }

    lhs <- paste(l,collapse="+")
    formula.char <- paste(lhs,r,sep="~")
    dcast(data,formula=as.formula(formula.char),...)
}
