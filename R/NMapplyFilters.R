##' Translate Nonmem filters to R code and apply to data
##' @param data An input data object. Could be read with NMreadCsv or
##'     NMscanInput.
##' @param file Path to mod/lst file. Only one of file or lines
##'     to be given. See `?NMreadSection` for understanding when to use,
##'     file, or lines. Only used when `filters` not provided.
##' @param lines The mod/lst as character, line by line.
##' @param filters A `data.frame` with filters as returned by
##'     `NMreadFilters()`. If not supplied, filters will be read from
##'     `file`/`lines`.
##' @param invert Invert the filters? This means read what Nonmem
##'     would disregard, and disregard what Nonmem would read.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param quiet Don't report information along the way if no warnings
##'     or errors. Default is FALSE.
##' @details This is not bulletproof. Nested conditions are not
##'     supported altogether.
##' @return data with filters applied
##' @seealso NMreadFilters
##' @keywords internal
##' @family Nonmem

## Don't export. This is only being used by NMscanInput at this point.


NMapplyFilters <- function(data,file,lines,filters,invert=FALSE,as.fun,quiet) {
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    . <- NULL
    cond <- NULL
    type <- NULL
    variable <- NULL
    value <- NULL

    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    

### We leave meta data untouched. This part is due to a previous design of NMscanInput. 
    
    data.meta <- NMinfoDT(data)

    
    if(missing(filters)||is.null(filters)){
        if(missing(lines)) lines <- NULL
        if(missing(file)) file <- NULL
### this is assuming there is only one file, or that lines contains only one control stream.    
        lines <- getLines(file=file,lines=lines)

        filters <- NMreadFilters(lines=lines,
                                 filters.only=TRUE,as.fun="data.table")
    }

    
    
    ## translating single-charaters
    name.c1 <- colnames(data)[1]
    scs <- sub(paste0("IGN"," *=* *(.+)"),"\\1",filters[class=="single-char",cond])
    scs.all <- scs
    expressions.sc <- c()
    if(length(scs)&&grepl("@",scs)) {
### NM manual: @ means first non-blank is a-z or A-Z.
        expressions.sc <- c(expressions.sc,paste0("!grepl(\"^ *[A-Za-z]\",",name.c1,")"))
        scs <- scs[!grepl("@",scs)]
    }

    regstring <- "[[:punct:]]|[[:alpha:]]"

    
    if(length(scs)&&any(grepl(regstring,scs))) {
        
        scs2 <- regmatches(scs,regexpr(regstring,scs))
        ## expressions.sc <- c(expressions.sc,paste0("!grepl('^",scs2,"\",",name.c1,")"))
        ## expressions.sc <- c(expressions.sc,paste0("!grepl('^",scs2,"','",name.c1,"')"))
        expressions.sc <- c(expressions.sc,paste0("!grepl('^[",scs2,"]',`",name.c1,"`)"))
        scs <- scs[!grepl(regstring,scs)]
    }
    
    if(length(scs)) stop(paste0("Not all single-character IGNORE statements were translated. This is left: ",scs))


    ## expressions.list is not a list, it's a char vector with the filter "expressions", as R code
    
    ## translating expression-style ones - i.e. not single-char
    expressions.list <- c(paste0(
        NMcode2R(
            filters[class=="var-compare",cond]
        )
    ))

    ## replace single = with ==
    expressions.list <- sub("^([a-zA-Z]* *)=( *[0-9]+)$","\\1==\\2",expressions.list)
    ## (DOSE 10) means (DOSE==10) in NMTRAN. 
    expressions.list <- sub("([[:alpha:]]+) +([[:alnum:]]+)","\\1==\\2",expressions.list)

    vars.cond <- sub("([[:alnum:]])[^[:alnum:]]+.*","\\1",expressions.list)

    copy.data <- FALSE
    data.sc <- data
    if(length(vars.cond)){
        missings <- listMissings(data,cols=unique(vars.cond),quiet=TRUE,as.fun="data.table")
        if(!is.null(missings)&&nrow(missings)>0){
            message(paste("Missing values found in columns used for ACCEPT/IGNORE statements. Please double-check dimensions of resulting data set. If at all possible, consider using a unique row identifier to merge by and/or make sure values are not missing in these colums.\n",
                          paste(capture.output(
                              print(
                                  missings[,.N,by=.(variable,value)]
                              )
                          ),collapse="\n")))
            ## missings
            vars.missing <- missings[,as.character(unique(variable))]
### numeric variables
            if(length(vars.missing)){
                copy.data <- TRUE
                data.sc <- copy(data)
                col.row.sc <- tmpcol(data=data.sc,base="row.sc")
                data.sc[,(col.row.sc):=.I]
                for(var in vars.missing){
                    if(is.numeric(data.sc[,get(var)])){
                        data.sc[is.na(get(var)),(var):=0]
                    } else {
                        data.sc[is.na(get(var)),(var):=""]
                    }
                }
            }
        }
    }
    
    ## cond.combine affects expression filters, not sc
    cond.combine <- "|"
    ## remember to negate everything if the type is ignore
    
    type.condition <- ifelse(all(filters[,type=="IGN"]),"IGN","ACCEPT")
    if(type.condition=="IGN") {
        if(length(expressions.list)){
            ## expressions.list <- paste0("!(",expressions.list,")")
            expressions.list <- paste0("!",expressions.list)
        }
        cond.combine <- "&"
    }
    
    

    if(length(expressions.sc)) {
        conditions.all.sc <- paste0(expressions.sc,collapse="&")
    } else {
        conditions.all.sc <- "TRUE"
    }

    
    expressions.all <- NULL
    if(length(expressions.list)) {
        expressions.all <- paste0("(",paste(expressions.list,collapse=cond.combine),")")
    }

    
    
    if(invert) {

        conditions.all.sc <- paste("!(",conditions.all.sc,")")
        expressions.all <- paste("!",expressions.all)
        
        ## conditions.all.sc <- paste("!(",conditions.all.sc,")")
        ## if(!is.null(expressions.all)){
        ##     expressions.all <- paste("!",expressions.all)
        ##     data <- as.data.table(data)[eval(parse(text=paste(conditions.all.sc,"|",expressions.all)))]
        ## } else {
        ##     data <- as.data.table(data)[eval(parse(text=conditions.all.sc))]
        ## }
        
    } ##else {
    ## apply sc first
    
    
    data.sc <- as.data.table(data.sc)[eval(parse(text=conditions.all.sc))]
    
    ## then lists
    if(!is.null(expressions.all)){
        data.sc <- as.data.table(data.sc)[eval(parse(text=expressions.all))]
    }
    ## }
    if(copy.data){
        
        data <- data[data.sc[,get(col.row.sc)]]
    } else {
        data <- data.sc
    }

    
    data.meta.filters <- NULL
    ## conds.sc <- NULL
    if(nrow(filters[class=="single-char"])){
        conds.sc <- paste(filters[class=="single-char",paste0(type,"=",cond)],collapse=", ")
        data.meta.filters <- data.table(
            nonmem=c(conds.sc),
            R=c(conditions.all.sc)
        )   
    }
    ## conds.text <- NULL
    if(nrow(filters[class=="var-compare"])){
        conds.text <- paste0(type.condition,": ",paste(filters[class=="var-compare",cond],collapse=", "))
        data.meta.filters <- rbind(data.meta.filters,
                                   data.table(
                                       nonmem=c(conds.text),
                                       R=c(paste(expressions.all,collapse=", "))
                                   )
                                   )
    }
    
    ## data.meta.filters <- data.table(
    ##     nonmem=c(
    ##         ## conditions.all.sc
    ##         conds.sc
    ##        ,
    ##         conds.text),
    ##     R=c(conditions.all.sc,paste(expressions.all,collapse=", "))
    ## )
    if(nrow(data.meta.filters)){
        data.meta$input.filters <- data.meta.filters
    }
    
    data <- as.fun(data)
    
    writeNMinfo(data,meta=data.meta,append=TRUE)
    data
    
}
