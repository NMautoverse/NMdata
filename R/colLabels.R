##' Extract column labels as defined in SAS
##' @param x object with elements containing label attributes.
##' @param sort If sort="alpha", results are sorted alphabetically.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function in as.fun to convert to something else. If
##'     data.tables are wanted, use `as.fun="data.table"`. The default
##'     can be configured using `NMdataConf()`.
##' @return A data.frame with variable and their labels
##' @seealso compareCols NMinfo
##' @import data.table
##' @export 

colLabels <- function(x,sort="alpha",as.fun){
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    ## compareCols(...,fun.class=function(x)attributes(x)$label)
    res <- dtapply(x,function(x)attributes(x)$label,as.fun="data.table",element.name="Column",value.names="Label")
    
    if(is.character(sort) && length(sort)==1 &&
       tolower(cleanSpaces(sort))=="alpha"){
        setorder(res ,"Column")
    }
    ##setnames(res,c("name","res"),c("Column","Label"),skip_absent = T)
    as.fun(res)
}
