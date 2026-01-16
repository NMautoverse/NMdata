##' Apply function of subsets of `data.frame`, return named list
##'
##' Based on columns in data, run function on subsets of data and
##' return the results in a list, carrying names of the subsets. Say a
##' column is `model` and you want to create a plot, run a regression
##' or anything on the data for each model. In that case
##' lapplydt(data,by="model",fun=function(x)lm(lAUC~lDose,data=x)). The
##' l in lapplydt is because a list is returned (like lapply), the dt
##' is because the input is a data.table (anything that can be converted to
##' such is OK).
##'
##' 
##' @param data Data set to process. Must be a data.frame-like structure.
##' @param by Column to split data by.
##' @param fun function to pass to `lapply()`. If an argument called `.nm` is
##'     defined, it gets a special meaning and can be used to retrieve the name
##'     of the respective subset. See examples.
##' @param drop.null If some subsets return `NULL`, drop the empty elements in the
##'     returned list?
##' @details the name of the current dataset can be reached with the `.nm`
##'     variable, if such argument is defined in `fun`. Se examples.
##' 
##' @import data.table
##' @examples
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' lapplydt(pk,by="DOSE",fun=nrow)
##' lapplydt(pk,by="DOSE",fun=function(x,.nm) {
##'     message("this is subset",.nm)
##'     message(paste("Result:",nrow(x)))
##' })
##' @return a list 
##' @export


## lapplydt <- function(data,by,fun,drop.null=FALSE){

##     if(!is.data.table(data)) data <- as.data.table

##     dt.split <- split(data,by=by,drop=TRUE)
##     nms.by <- names(dt.split)

##     res.l <- lapply(nms.by,function(.nm){

##         dt.m <- dt.split[[.nm]]

##         ## Make a copy of the function and inject `.nm` into its environment
##         fun2 <- fun
##         parent_env <- environment(fun2)
##         if (is.null(parent_env)) {
##             parent_env <- emptyenv()  ## safe fallback for primitives
##         }
##         env <- new.env(parent = parent_env)
##         env$.nm <- .nm
##         environment(fun2) <- env

##         fun2(dt.m)

##     })

##     names(res.l) <- nms.by
##     if(drop.null){
##         res.l <- res.l[!sapply(res.l,is.null)]
##     }

##     res.l
## }


lapplydt <- function(data, by, fun,drop.null=FALSE) {
    if (!is.data.table(data)) data <- as.data.table(data)
    
    dt.split <- split(data, by = by, drop = TRUE)
    nms.by <- names(dt.split)
    
    res.l <- lapply(nms.by, function(.nm) {
        dt.sub <- dt.split[[.nm]]
        
        ## Only pass .nm if fun has it as a formal argument
        if (".nm" %in% names(formals(fun))) {
            fun(dt.sub, .nm = .nm)
        } else {
            fun(dt.sub)
        }
    })
    names(res.l) <- nms.by

    if (drop.null) {
        res.l <- res.l[!vapply(res.l, is.null, logical(1))]
    }

    res.l

}
