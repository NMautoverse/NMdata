##' read lines as needed
##'
##' Functions that take file and lines arguments can use this function to derive
##' lines no matter what was provided.
##' @param file A file path to a a text file to read.
##' @param lines Text lines if file was already read.
##' @param ensure.ascii If `TRUE`, the result is converted to ascii characters.
##'     This is done to ensure some further processing will work but you may
##'     lose say greek characters from comments.
##' @param as.one If the `file` argument is used and if `as.one` is TRUE, the
##'     file(s) are read and put into a `data.table` with a model column and a
##'     `text` column. Default is FALSE. Be careful with this, as it returns
##'     different formats whether the `file` or the `lines` argument is used.
##' 
##' @keywords internal

getLines <- function(file,lines,linesep="\n",simplify=TRUE,ensure.ascii=TRUE,col.model,modelname,as.one){

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL
    if(missing(as.one)) as.one <- NULL
    if(is.null(as.one)) as.one <- FALSE

    fun.ens.ascii <- function(x) iconv(x,"latin1", "ASCII", sub="")
    
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)
    

    if(!xor(is.null(file),is.null(lines))) stop("Exactly one of file or lines must be supplied")
    
    if(!is.null(file)) {
        if(!all(file.exists(file))) stop("When using the file argument, file has to point to an existing file.")

        lines <- lapply(file,function(ff){
            readLines(ff,warn=FALSE) |>
                fun.ens.ascii()
        })

    }

    if(!is.list(lines)) lines <- list(lines)

    if(is.null(file)&&ensure.ascii){
        lines <- lapply(lines,fun.ens.ascii)
    }
    
    ## this drops trailing empty lines
    if(!isFALSE(linesep)) {
        lines <- lapply(lines,function(x){
            strsplit(paste(x,collapse=linesep),split=linesep)[[1]]
        })
    }

    if(as.one && !is.null(file)){
        
        names.models <- modelname(file)
        lines <- lapply(1:length(file),function(N)data.table( text=lines[[N]])[,(col.model):=names.models[N]])
        ##lines[[N]][,(modelname):=names.models[N]])
        lines <- rbindlist(lines)
    }
    
    if(simplify && length(lines)==1){
        lines <- lines[[1]]
    } 
    
    lines

}
