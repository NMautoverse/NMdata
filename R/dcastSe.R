##' A standard-evaluation interface to `data.table::dcast()
##'
##' data.table's dcast() transforms from long to wide format. It lacks a standard-evalutation interface for programming, and dcastSe() is an attempt to fill that gap, still using data.table::dcast() to perform the operation.
##'
##' @param data data set (data.frame) to transpose (widen). 
##' @param l left-hand side variables as character vector. Result will be
##'   long/vertical in these variables. The elements in `l` can be named in
##'   which case the resulting columns will be renamed accordingly.
##' @param r left-hand side variables as character vector. Result will be wide
##'   in these variables.
##' @param as.fun An optional function to convert results with. If `data` is a
##'   `data.table`, the default is to return a `data.table`, and if not the
##'   default is to return a `data.frame`.
##' @param ... Additional arguments passed to `data.table::dcast()`.
##' @details Only two of l, r, and value.var are needed (see ?data.table::dcast
##'   for value.var), and the others will be derived. However, if value.var is a
##'   function, both l and r are needed.
##' @import data.table
##' @importFrom stats as.formula
##' @return a data.table
##' @examples
##' dt <- data.frame(
##'   id = rep(1:3, each = 2),
##'   time = rep(c(0, 1), 3),
##'   value = 1:6
##' )
##'     
##' # Cast to wide format
##' dcastSe(dt, l = "id", r = "time", value.var = "value")
##' # rename a "left" column
##' dcastSe(dt, l = c(Subject="id"), r = "time", value.var = "value")
##' @export

dcastSe <- function(data,l,r,as.fun,...){

    if(!is.data.table(data)){
        data <- as.data.table(data)
    }

    if(missing(l)) l <- NULL
    if(missing(r)) r <- NULL

    if(missing(as.fun)) as.fun <- NULL

    if(is.data.table(data) && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

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
    if(!length(l)) stop("No left-hand side. If you are sure what you are doing is valid, it's not supported by dcastSe().")
if(!length(r)) stop("No right-hand side. If you are sure what you are doing is valid, it's not supported by dcastSe().")
    
    lhs <- paste(l,collapse="+")
    rhs <- paste(r,collapse="+")
    formula.char <- paste(lhs,rhs,sep="~")
  res <- dcast(data,formula=as.formula(formula.char),...)

  names.l <- names(l)
  if(!is.null(names.l)){
    rename.from <- l[names.l!=""]
    rename.to <- names.l[names.l!=""]
    setnames(res,rename.from,rename.to)
  }

    as.fun(res)

}
