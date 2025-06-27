
##' Read SIZES info from a control stream
##' @param file.mod Control stream path.
##' @param lines Character vector with control stream file.
##' @return A list with SIZES parameter values
##' @export


NMreadSizes <- function(file.mod=NULL,lines=NULL){
    
    lines.sizes <- NMreadSection(file=file.mod,lines=lines,section="SIZES",keep.empty=F,keep.comments=FALSE,keep.name=FALSE)
    if(is.null(lines.sizes)) return(NULL)
    
    ## make it just one line
    lines.sizes <- gsub("\\n"," ",lines.sizes)
    lines.sizes <- paste(lines.sizes,collapse=" ")
    ## Replace all " +" by " " (multiple spaces to just one space)
    lines.sizes <- cleanSpaces(lines.sizes)
    ## clean the found SIZES first to make sure " *=" and "= *" are replaced by "=".
    lines.sizes <- gsub(" *= *","=",lines.sizes)
    ## Then strsplit with " " as separator should identify parameters and values.
    strings.sizes <- strsplit(lines.sizes,split=" ")[[1]]
    ## We now have strings "par1=val1" and "par2=val2". These have to be separated - could be done by strsplit again, this time using = as separator.
    list.sizes.strings <- lapply(strings.sizes,function(x)strsplit(x,split="=")[[1]])
    list.sizes.lists <- lapply(list.sizes.strings,function(x)setNames(list(x[2]),x[1]))
    list.sizes <- do.call(c,list.sizes.lists)

    list.sizes
}
