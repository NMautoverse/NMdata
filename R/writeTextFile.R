##' Conveniently write text lines to file
##'
##' @param lines the character lines to write
##' @param file The file name path to write to
##' @return NULL
##' @keywords internal

writeTextFile <- function(lines,file){

    if(is.list(lines)){
        all.files <- nameMultipleFiles(file,lines)
        lapply(1:length(all.files),function(x){
            writeTextFile(lines=lines[[x]],file=all.files[x])
        })
    }

    con <- file(file, "wb")
    writeLines(lines, con = con)
    close(con)
}
