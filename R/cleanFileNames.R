##' Drop spaces and odd characters. Use to ensure generated file names are
##' usable.
##' @param x a string to clean
##' @param allow.slash Allow slashes? Probably set to TRUE if x can be a
##'     directory.
##' @return A character vector
##' @keywords internal
##' @examples
##' cleanFileNames("e w% # ff!l3:t,3?.csv")
##' cleanFileNames("3!?:#;<>=, {}|=g+&-
##' .csv")
##' @export
##'

cleanFileNames <- function(x,allow.slash=FALSE){


    x <- gsub("[ +!?#:;<>&,\\{\\}\\|=\\(\\)]", "",x) 
    if(!allow.slash){
        x <- gsub("/", "",x) 
    }
    x <- gsub(pattern="-",replacement="",x=x,perl=TRUE) 
    x <- gsub(pattern="\n",replacement="",x=x)
    
    x
}
