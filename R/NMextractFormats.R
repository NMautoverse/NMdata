##' @keywords internal

NMextractFormats <- function(ctl,section=NULL,as.dt=FALSE) {
    
    #### Section start: Dummy variables, only not to get NOTE's in pacakge checks  ####

    line <- NULL
    type <- NULL

    ### Section end: Dummy variables, only not to get NOTE's in pacakge checks 
    

    lines <- as.NMctl(ctl,warn=FALSE)
    dt.format <- data.table(line=lines[grepl("^[ ;]+(format|parstext)(\\.[a-zA-Z])*",lines,ignore.case = TRUE)])
        
    dt.format[,line:=sub("^[ ;]+(parstext|format)", "; format", line, ignore.case = TRUE, perl = TRUE)]

    dt.format[,type:=tolower(line)]
    dt.format[,type:=sub("parstext","format",type)]
    ## classify them as format, format.omega, format.sigma
    dt.format[,type:=sub("^[ ;]+(format\\.[a-zA-Z]+).*","\\1",type)]
    dt.format[!type%in%c("format.omega","format.sigma"),type:="format"]

    ## take first of each
    ### todo look for duplicates, warn, remove
    dt.format[,first:=!duplicated(type)]
    
    
    ## Derive formats 
    dt.format[,format:=sub("^[ ;]+format(\\.[a-zA-Z]*)*[[:punct:]]* *","",line)]
    
    
    if(as.dt){
        return(dt.format[])
    }
    
    res.list <- with(dt.format[first==TRUE],setNames(as.list(format),type))
    res.list
    
}


