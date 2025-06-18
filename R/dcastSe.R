##' A standard-evaluation interface to `data.table::dcast()`
##'
##' @param data data set to transpose (widen)
##' @param l left-hand side variables as character vector. Result will
##'     be long/vertical in these variables.
##' @param r left-hand side variables as character vector. Result will
##'     be wide in these variables.
##' @param ... Additional arguments paseed to `data.table::dcast()`.
##' @importFrom data.table dcast
##' @importFrom stats as.formula
##' @keywords internal 

dcastSe <- function(data,l,r,...){
    lhs <- paste(l,collapse="+")
    formula.char <- paste(lhs,r,sep="~")
    dcast(data,formula=as.formula(formula.char),...)
}
