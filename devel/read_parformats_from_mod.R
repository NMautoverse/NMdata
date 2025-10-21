funFormat <- function(file,section=NULL) {
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@213@"]]));##:ess-bp-end:##

    lines <- readLines(file,warn=FALSE)
    dt.format <- data.table(line=lines[grepl(" *;+ format(\\.[a-zA-Z])*",lines)])
    ## classify them as format, format.omega, format.sigma
    dt.format[,type:=sub(" *;+ *format\\.*([a-zA-Z]+).*","\\1",line)]
    dt.format[,type:=tolower(type)]
    dt.format[!type%in%c("omega","sigma"),type:=""]
    ## take first of each

    dt.format[,first:=!duplicated(type)]

    ## Derive formats 
    dt.format[,format:=sub]

    ## organize in list (or dt?)
    list(format=format,
         format.omega=format.omega)
}


if(F){
    file.mod <- "/data/home/philipde/wdirs/suitcase/sandbox/correlate_without_paired_obs/nonmem/run011.mod"
    funFormat(file.mod)

}
