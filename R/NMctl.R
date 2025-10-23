
##' Read as class NMctl
##' @param x object to read.
##' @param ... Not used.
##' @return An object of class `NMctl`.
##' @keywords internal
readCtl <- function(x,...){
    UseMethod("readCtl",x)
}

##readCtl(x="run1.mod")

##' @import NMdata
##' @keywords internal
readCtl.character <- function(x,...){
    
    ## ctl <- NMreadSection(file=x)
    
    dots <- list(...)
    if("lines"%in%names(dots)){
        
        ### this only supports one ctl. Generalize?
        x2 <- do.call(c,strsplit(x,"\\n"))
        ctl <- x2
    } else {
        
        ctl <- readLines(x,warn=FALSE)
    }
    ## class(ctl) <- "NMctl"
    setattr(ctl,"class",unique(c("NMctl",class(ctl))))
    ctl
}

##' @keywords internal
readCtl.NMctl <- function(x,...){
    x
}

##' @keywords internal
print.NMctl <- function(x,...) {
   
    if(!is.list(x)) x <- list(x)
    res <- lapply(x,function(x){
        x <- x[!grepl("^ *$",x)]
        paste0(paste(x,collapse="\n"),"\n\n")
    })
    cat(do.call(paste0,res))
}

##' Convert object to class NMctl
##' @param x object to convert
##' @param ... Not used
##' @return An object of class `NMctl`.
##' @keywords internal
as.NMctl <- function(x,...){
    UseMethod("as.NMctl",x)
}

##' @keywords internal
as.NMctl.character <- function(x,...){
    readCtl(x,...)
}

##' @keywords internal
as.NMctl.NMctl <- function(x,...){
    x
}
