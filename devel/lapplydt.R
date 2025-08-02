##' apply function of subsets of dt, return named list
##'
##' @param data
##' @param by
##' @param fun
##'
##' @keywords internal


lapplydt <- function(data,by,fun){

    dt.split <- split(data,by=by,drop=TRUE)
    nms.by <- names(dt.split)

    
    tabs.sub.w <- lapply(nms.by,function(nm){
        
        dt.m <- dt.split[[nm]]
        with(list(nm=nm),fun(dt.m))
    })

    names(tabs.sub.w) <- nms.by
    tabs.sub.w
}
