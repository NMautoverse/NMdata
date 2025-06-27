
##' Read Nonmem table files without assumptions about what tables they
##' contain
##'
##' @param file A Nonmem table file. Can be output tables, or one of
##'     the several different results files from Nonmem.
##' @param col.table.name Name of the column (to be created)
##'     containing the "table name" which is derived from the Nonmem
##'     description of the table sometimes pasted above the table
##'     data.
##' @import data.table
##' @details `NMreadTabSlow` reads parameter tables from Nonmem very
##'     slowly, and most often `NMreadTab` is a better function to
##'     use. However, `NMreadTabslow` also works for table files that
##'     contain incompatible tables.
##' @keywords internal
##' 
### Polishing needed. Don't export.

NMreadTabSlow <- function(file,col.table.name=TRUE){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    TABLENO <- NULL
    end.idx <- NULL
    start.idx <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    rm.col.table.name <- FALSE
    if(missing(col.table.name) || is.null(col.table.name) || isFALSE(col.table.name) ){
        col.table.name <- "table.name"
        rm.col.table.name <- TRUE
    }
    if(isTRUE(col.table.name)){
        col.table.name <- "table.name"
    }
    
    lines <- readLines(file)
    idx.tabstart <- grep("^TABLE NO",lines)
    dt.ts2 <- data.table(idx=c(idx.tabstart,length(lines)+1))
    dt.ts3 <- data.table(start=dt.ts2[-.N],end=dt.ts2[-1]-1)
    dt.ts3[,TABLENO:=sub(" *TABLE NO\\. +([1-9][0-9]*).*","\\1",lines[start.idx])]
    dt.ts3[,(col.table.name):=sub("^ *TABLE NO\\. *([1-9][0-9]*) *: *(.*)$","\\2",lines[start.idx])]
    

    list.res <- lapply(
        split(dt.ts3,by=c("TABLENO",col.table.name))
       ,function(x){
           
           x[,fread(text=lines[(start.idx+1):end.idx]),by=c("TABLENO",col.table.name)]
           })
    ## lapply(1:length(list.res),function(N)list.res[[N]][
    ##                                         ,TABLENO:=unique(list.name])

    list.res
}
