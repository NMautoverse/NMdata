##' Write IGNORE/ACCEPT filters to NONMEM model
##' @param file Path to control stream. Use `file` or `lines`.
##' @param lines Control stream as text lines. Use `file` or `lines`.
##' @param filters A data frome with filters, like returned by
##'     `NMreadFilters()`.
##' @param write If `file` is provided, write the results to file? If
##'     `lines` is used, `write` cannot be used.
##' @return Resulting control stream (lines) as character vector 
##' @export

NMwriteFilters <- function(file=NULL,lines=NULL,filters,write){

    cond <- NULL
    text <- NULL
    type <- NULL
    
    if(missing(write) || is.null(write)){
        write <- !is.null(file)
    }

    lines <- getLines(file=file,lines=lines)
    text.no.filters <- NMreadFilters(lines=lines,filters.only=FALSE)$text.nofilters


    if(is.data.frame(filters)){
        if(!is.data.table(filters)){
            filters <- as.data.table(filters)
        }
        filters[,row:=.I]
        
        filters[class=="single-char",
                text:=sprintf("%s=%s",type,cond)
                ]

        filters[class=="var-compare",
                text:=sprintf("%s(%s)",type,cond)
                ]
    } else if (is.character(filters)){
        filters <- paste(filters,collapse = " ")
    }
    
    ##newdata <- sprintf("%s\n%s\n",text.no.filters,paste(filters$text,collapse="\n"))
    newdata <- c(text.no.filters,filters$text)
    ## newdata <- writeLines(newdata)



    res <- NMwriteSectionOne(lines=lines,newfile=file,section="data",newlines=newdata,
                                      backup=FALSE,quiet=TRUE,write=write)


    res

}

