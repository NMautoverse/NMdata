##' Extract column labels as defined in SAS
##' @param ... See `?compareCols`
##' @return A data.frame with variable and their labels
##' @seealso compareCols NMinfo
##' @import data.table
##' @export 

colLabels <- function(x,sort="alpha"){
    ## compareCols(...,fun.class=function(x)attributes(x)$label)
    res <- dtapply(x,function(x)attributes(x)$label)
    
    if(is.character(sort) && length(sort)==1 &&
       tolower(cleanSpaces(sort))=="alpha"){
        setorder(res ,"name")
    }
    setnames(res,c("name","res"),c("Column","Label"),skip_absent = T)
    res
}
