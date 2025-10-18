##' Apply function and return a data.frame
##'
##' A convenience function that returns a data.frame with a column
##' representing the input values and a column with results. This is
##' still experimental and will not work for many input structures.
##'
##' @param X Like for `lapply()`, an object to process (typically a
##'     vector or a list). Passed to `lapply()`.
##' @param FUN Function to run for each element in `X`. Passed to
##'     `lapply()`.
##' @param value.names If supplied, setnames will be run on each
##'     element returned by lapply wit value.names as the `new`
##'     argument.
##' @param element.name What to call the column holding the values or
##'     the names of `X`? Default is "element". What goes into this
##'     column depends on the class of `X`. If `X` is a character
##'     vector, it will be the values of `X`. If `X` is a list, it
##'     will be the names of the elements in the list.
##' @param fill Fill resulting data with `NA`s in order to combine
##'     into a single `data.frame`? Default is TRUE.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function in as.fun to convert to something else. If
##'     data.tables are wanted, use `as.fun="data.table"`. The default
##'     can be configured using `NMdataConf()`.
##' @param ... arguments passed to lapply
##' @import data.table
##' 
##' @details Only functions that return vectors are currently
##'     supported. dtapply should support functions that return
##'     data.frames.
##' @return a data.table
##' @export
##'
## generally applicable but still too early to export

### examples
## NMsim::dtapply(setNames(1:4,letters[1:4]),sqrt)

dtapply <- function(X,FUN,value.names=NULL,element.name="element",fill=TRUE,as.fun,...){

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    name <- NULL

    nms.x <- names(X)
    if(is.null(nms.x)){
        if(is.character(X)) {
            nms.x <- X
        } else {
            nms.x <- as.character(1:length(X))
        }
    }
    ## todo run in try
    res.list <- lapply(X,FUN,...)

    ## todo make dt of elements. 
    ## todo convert all to data.table
    res.list <- lapply(res.list,as.data.table)
    ## todo remove NULL and zro row elements

    
    if(!is.null(value.names)){
        res.list <- lapply(res.list,setnames,new=value.names)
    }
    
    res.list <- lapply(1:length(res.list),function(I){
        res.list[[I]][,name:=nms.x[I]]
        res.list[[I]]
    })
    
    res <- rbindlist(res.list,fill=fill)
  
    setnames(res,"name",element.name)
    setcolorder(res,element.name)

    as.fun(res)
}
