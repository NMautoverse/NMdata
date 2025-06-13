##' Create file names for multiple objects
##'
##' Can currently only append a counter to file name stem.
##' 
##' @param fn A file name to use as the stem for new file names
##' @param list.obj List of objects to create file names for
##' @keywords internal

nameMultipleFiles <- function(fn,list.obj){
    length.num.char <- length(list.obj)
    submodels <- sprintf(fmt=paste0("%0",length.num.char,"d"),1:length.num.char)
    path.sim <- fnAppend(fn,submodel)

    path.sim
}
