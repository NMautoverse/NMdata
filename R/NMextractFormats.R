##' @keywords internal

NMextractFormats <- function(ctl,section=NULL,as.dt=FALSE) {
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks  ####

    line <- NULL
    type <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks 

    
    lines <- as.NMctl(ctl,warn=FALSE)
    dt.format <- data.table(line=lines[grepl(" *;+ *format(\\.[a-zA-Z])*",lines)])
    ## classify them as format, format.omega, format.sigma
    dt.format[,type:=sub(" *;+ *(format\\.*[a-zA-Z]+).*","\\1",line)]
    dt.format[,type:=tolower(type)]
    dt.format[!type%in%c("format.omega","format.sigma"),type:="format"]
    ## take first of each

    dt.format[,first:=!duplicated(type)]


### todo look for duplicates, warn, remove
    
    
    ## Derive formats 
    ## dt.format[,format:=sub(" *;+ format.*","",line)]
    ## dt.format[,format:=regmatches(line, regexpr("%.*", line))]
    ## dt.format[,format:=regmatches(line, regexpr("%.*", line))]
    dt.format[,format:=sub(" *;+ *format(\\.[a-zA-Z])*[[:punct:]]* *","",line)]
    
    ## organize in list (or dt?)
    ## list(format=format,
    ##      format.omega=format.omega)
    
    if(as.dt){
        return(dt.format[])
    }
    
    res.list <- with(dt.format[first==TRUE],setNames(as.list(format),type))
    res.list
    
}


if(FALSE){
    ## library(devtools)

    
    ##     file.mod <- "/data/home/philipde/wdirs/suitcase/sandbox/correlate_without_paired_obs/nonmem/run011.mod"

    ##     load_all("~/wdirs/NMdata")
    ##     lines <- ";;format %init;%idx;%symbol;%trans
    ## ;;format.omega %init;%idx;%symbol"
    ##     ctl <- as.NMctl(x=lines,lines=TRUE)
    ##     NMextractFormats(ctl)

}
