##' Conveniently write text lines to file
##' @param lines the character lines to write
##' @param file The file name path to write to
##' @return NULL
##' @keywords internal

writeTextFile <- function(lines,file){
    con <- file(file, "wb")
    writeLines(lines, con = con)
    close(con)
}
