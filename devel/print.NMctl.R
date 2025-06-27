## for now, x must be a list of sections. I think for a NMctl class we
## need at least a preamble section too. Also, it should be aware of
## the order of the sections? Make list of what NMctl is going to be
## used for. And then, what functions are wanted to work with that
## class, other than print.NMctl?

## seems like NMreadSection should have a cleanup functionality too. Maybe it should return both plain original text and cleaned up code? Or is that a print thing? NMrelate() also reproduces tabulator characters. Seems like we want to clean up this code with just one function.

print.NMctl <- function(x) {

    
    if(!is.list(x)) x <- list(x)
    res <- lapply(x,function(x){
        x <- x[!grepl("^ *$",x)]
        paste(paste(x,collapse="\n"),"\n\n")

    })
    cat(do.call(paste,res))
}
