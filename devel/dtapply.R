##' Apply function and return a data.table
##'
##' A convenience function that returns a data.table with a column
##' representing the input values and a column with results. This is
##' still experimental and will not work for many input structures.
##' 
##' @param ... arguments passed to lapply
##' @import data.table 
##' 
##' @details Only functions that return vectors are currently
##'     supported. dtapply should support functions that return
##'     data.frames.
##' @keywords internal
##' @return a data.table
##'
## generally applicable but still too early to export

### examples
## NMsim:::dtapply(setNames(1:4,letters[1:4]),sqrt)

dtapply <- function(X,FUN,value.names=NULL,element.name="element",...){
    nms.x <- names(X)
    res.list <- lapply(X,FUN,...)

    
    ## res <- data.table(name=nms.x,res=unlist(res.list))
    
    res.list <- lapply(res.list,as.data.table)
    if(!is.null(value.names)){
        res.list <- lapply(res.list,setnames,new=value.names)
    }
    res.list <- lapply(nms.x,function(I)res.list[[I]][,name:=I])
    res <- rbindlist(res.list,fill=TRUE)
    setnames(res,"name",element.name)
    setcolorder(res,element.name)
    res
}
