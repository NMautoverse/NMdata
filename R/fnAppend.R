##' paste something before file name extension.
##' 
##' Append a file name like file.mod to file_1.mod or file_pk.mod. If
##' it's a number, we can pad some zeros if wanted. The separator
##' (default is underscore) can be modified.
##' @param fn The file name or file names to modify.
##' @param x A character string or a numeric to add to the file name
##' @param pad0 In case x is numeric, a number of zeros to pad before
##'     the appended number. This is useful if you are generating say
##'     more than 10 files, and your counter will be 01, 02,..,
##'     10,... and not 1, 2,...,10,...
##' @param sep The separator between the existing file name (until
##'     extension) and the addition.
##' @param collapse If `x` is of length greater than 1, the default is
##'     to collapse the elements to a single string using `sep` as
##'     separator. See the `collapse` argument to `?paste`. If you
##'     want to treat them as separate strings, use `collapse=NULL`
##'     which will lead to generation of separate file names. However,
##'     currently `fn` or `x` must be of length 1.
##' @param allow.noext Allow `fn` to be string(s) without extensions?
##'     Default is `FALSE` in which case an error will be thrown if
##'     `fn` contains strings without extensions. If `TRUE`, `x` will
##'     be appended to fn in these cases.
##' @return A character (vector)
##'
##' @examples
##' fnAppend("plot.png",1)
##' fnAppend("plot.png",1,pad0=2,sep="-")
##' fnAppend("plot.png","one")
##' fnAppend("plot","one",allow.noext=TRUE)
##' @export


fnAppend <- function(fn,x,pad0=0,sep="_",collapse=sep,allow.noext=FALSE){
    
    if((!is.numeric(x)&&!is.character(x))) stop("x must be numeric or character vector.")
    
   
    if(is.numeric(x)){
        string <- sprintf(fmt=paste("%0",pad0,"d",sep=""),x)
    } else {
        string <- x
    }
    string <- paste(string,collapse=collapse)

    if(length(fn)>1 && length(string)>1){
        stop("Both fn and x are of length>1. This is currently not supported.")
    }
    
    if(all(nchar(string))==0) return(fn)
    
    has.ext <- grepl(".*[^\\.]\\.[a-zA-Z0-9]+",fn)
    if( !all(has.ext) && !allow.noext){
        stop("Elements in fn have no extension and allow.noext=FALSE")
    }
        
    allext <- rep("",length(fn))
    allext[has.ext] <- sub(".*[^\\.]\\.([a-zA-Z0-9]+)$","\\1",fn[has.ext])
    allext[has.ext] <- paste0(".",allext[has.ext])

    fnroot <- sub(paste0("\\.[a-zA-Z0-9]+$"),"",fn)
    
   
    
    paste0(fnroot,sep,string,allext)
        
}
