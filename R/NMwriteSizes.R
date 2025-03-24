##' Create or update $SIZES in a control stream
##'
##' Update $SIZES parameters in a control stream. The control stream
##' can be in a file or provided as a character vector (file lines).
##' 
##' @param file.mod A path to a control stream. See also alternative
##'     `lines` argument. Notice, if `write` is `TRUE` (default) and
##'     `newfile` is not provided, `file.mod` will be overwritten.
##' @param newfile An optional path to write the resulting control
##'     stream to. If nothing is provided, the default is to overwrite
##'     `file.mod`.
##' @param lines Control stream lines as a character vector. If you
##'     already read the control stream - say using
##'     `NMdata::NMreadSection()`, use this to modify the text lines.
##' @param wipe The default behavior (`wipe=FALSE`) is to add the
##'     `$SIZES` values to any existing values found. If SIZES
##'     parameter names are overlapping with existing, the values will
##'     be updated. If `wipe=TRUE`, any existing `$SIZES` section is
##'     disregarded.
##' @param write Write results to `newfile`?
##' @param ... The $SIZES parameters. Provided anything, like `PD=40`
##'     See examples.
##' @return Character lines with updated control stream
##' @examples
##' ## No existing SIZES in control stream
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr132.mod",package="NMdata")
##' newmod <- NMwriteSizes(file.mod,LTV=50,write=FALSE)
##' head(newmod)
##' }
##' ## provide control stream as text lines
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr032.mod",package="NMdata")
##' lines <- readLines(file.mod)
##' newmod <- NMwriteSizes(lines=lines,LTV=50,write=FALSE)
##' head(newmod)
##' }
##' ## By default (wipe=FALSE) variabels are added to SIZES 
##' \dontrun{
##' lines.mod <- NMwriteSizes(file.mod,LTV=50,write=FALSE) 
##' newmod <- NMwriteSizes(lines=lines.mod,PD=51,write=FALSE)
##' head(newmod)
##' }
##' 
##' @importFrom utils modifyList
##' @export

NMwriteSizes <- function(file.mod=NULL,newfile,lines=NULL,wipe=FALSE,write=!is.null(newfile),...){
    
    
    sizes.new <- list(...)
### check sizes.new
    ## all elements must be named.
    ## Names must not contain spaces.
    ## all elements numeric?
    
    if(wipe){
        sizes.old <- NULL
    } else {
        sizes.old <- NMreadSizes(file.mod=file.mod,lines=lines)
    }
    
### combine old and new sizes. modifyList prioritizes last list:
    ## modifyList(list(a=1,b=1),list(b=2,c=2))
    if(!is.null(sizes.old)){
        sizes.new <- modifyList(sizes.old,sizes.new)
    }
    
    ## convert sizes.new into text lines for control stream
    lines.new <- paste(c(
        "$SIZES",
        paste(names(sizes.new),sizes.new,sep="=")
    ),collapse=" "
    )
    
    # create lines from file.mod if not passed in as 'lines'
    if(!is.null(file.mod) && is.null(lines)) {
        lines <- readLines(file.mod,warn=FALSE)
    } 
    
    # if model already has $SIZES and we want to overwrite or append to it.
    if(!is.null(sizes.old)) {
        # if we want to overwrite it or append to it, sizes.new and lines.new will already be modified for that, so replace
        textlines = NMwriteSectionOne(lines = lines,section="SIZES",newlines=lines.new,location="replace",quiet=TRUE)

    # else if model does not have $SIZES, create it
    } else if (is.null(NMreadSizes(lines=lines))) {
        textlines = NMwriteSectionOne(lines=lines,section="SIZES",newlines=lines.new,location="first",quiet=TRUE)
        
    # else if it had $SIZES but we wiped it, and we want to replace/append it:
    } else if (!is.null(NMreadSizes(lines=lines)) && wipe) {
        textlines = NMwriteSectionOne(lines=lines,section="SIZES",newlines=lines.new,location="replace",quiet=TRUE)
    }
    
    if(write && !is.null(newfile)) {
        writeTextFile(textlines, newfile)
    } else {
        return(textlines)
    }

}
