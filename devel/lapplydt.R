##' apply function of subsets of dt, return named list
##'
##' 
##' @param data
##' @param by
##' @param fun
##' @details the name of the current dataset can be reached with the
##'     .nm variable.
##' @import data.table
##' @keywords internal


lapplydt <- function(data,by,fun){

    dt.split <- split(data,by=by,drop=TRUE)
    nms.by <- names(dt.split)
    
    res.l <- lapply(nms.by,function(nm){
        dt.m <- dt.split[[nm]]
        
        ## Create a new function environment that contains `nm`
        fun2 <- fun
        env <- new.env(parent = environment(fun2))
        env$nm <- nm
        environment(fun2) <- env

        fun2(dt.m)

    })

    names(res.l) <- nms.by
    res.l
}
