##' Create file names for multiple list elements
##' @param fn File name to provide stem for all file names
##' @param list.obj List of objects to provide names for
##' @param simplify If only one file path, skip numbering? Default is
##'     TRUE.
##' @keywords internal

nameMultipleFiles <- function(fn,list.obj,simplify=TRUE){
    submodel <- NULL
    
    length.num.char <- length(list.obj)
    if(!simplify || length.num.char>1){
        submodels <- sprintf(fmt=paste0("%0",length.num.char,"d"),1:length.num.char)
        fn <- fnAppend(fn,submodels)
    }

    fn
}
