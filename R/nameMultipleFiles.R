##' Create file names for multiple list elements
##' @param fn File name to provide stem for all file names
##' @param list.obj List of objects to provide names for
##' @keywords internal

nameMultipleFiles <- function(fn,list.obj){
    submodel <- NULL
    
    length.num.char <- length(list.obj)
    submodels <- sprintf(fmt=paste0("%0",length.num.char,"d"),1:length.num.char)
    ## pars[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),.GRP),by=bycols]
    path.sim <- fnAppend(fn,submodel)
    ## pars[,path.sim:=fnAppend(path.sim.0,submodel),by=.(ROW)]
    ##pars[,fn.sim:=basename(path.sim)]
    ##pars[,run.sim:=fnExtension(fn.sim,"")]

    path.sim
}
