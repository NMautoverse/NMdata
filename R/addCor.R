##' add correlations of off-diagonal OMEGA and SIGMA elements to a parameter table
##' @param pars A parameter table, like returned by `NMreadExt()`.
##' @param by The name of a column, as a string. Calculate the
##'     correlations within a grouping variable?  This will often be a
##'     column containing the model name.
##' @param as.fun See `?NMdataConf`
##' @param col.value The name of the column from which to take the
##'     `OMEGA` values. Default is "value" in alignment with the
##'     output from `NMreadExt()`.
##' @return The parameter table with a `corr` column added.
##' @import data.table
##' @importFrom stats cov2cor
##' @export
##' 
## Can be exported but needs as.fun and return

addCor <- function(pars,by=NULL,as.fun,col.value="value"){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    corr <- NULL
    i <- NULL
    j <- NULL
    par.type <- NULL
    value <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    pars <- as.data.table(pars)
    if(is.null(by)){
        pars.list <- list(pars)
    } else {
        pars.list <- split(pars,by=by)
    }
    
    res.list <- lapply(
        pars.list,
        function(x){

            x.omega <- x[par.type=="OMEGA"]
            dt.cor <- NULL
            if(nrow(x.omega)){
                Sigma <- dt2mat(x.omega,col.value=col.value)
                mat.cor <- suppressWarnings(cov2cor(Sigma))
                dt.cor.1 <- mat2dt(mat.cor,triangle="all",as.fun="data.table")[,par.type:="OMEGA"]
                ##x <- mergeCheck(x,dt.cor[,.(par.type="OMEGA",i,j,corr=get(col.value))],by=cc(par.type,i,j),all.x=TRUE,quiet=TRUE)
                dt.cor <- rbind(dt.cor,dt.cor.1)
            }
            
            x.sigma <- x[par.type=="SIGMA"]
            if(nrow(x.sigma)){
                Sigma <- dt2mat(x.sigma,col.value=col.value)
                mat.cor <- suppressWarnings(cov2cor(Sigma))
                dt.cor.1 <- mat2dt(mat.cor,triangle="all",as.fun="data.table")[,par.type:="SIGMA"]
                ## x <- mergeCheck(x,dt.cor[,.(par.type="SIGMA",i,j,corr=get(col.value))],by=cc(par.type,i,j),all.x=TRUE,quiet=TRUE)
                dt.cor <- rbind(dt.cor,dt.cor.1)
            }
            if(!is.null(dt.cor)){
                dt.cor <- dt.cor[,.(par.type,i,j,corr=get(col.value))]
                dt.cor[is.nan(corr),corr:=0]
                x <- mergeCheck(x,dt.cor,by=cc(par.type,i,j),all.x=TRUE,quiet=TRUE)
            }
            x
        })
    
    as.fun(rbindlist(res.list))
}



##' Deprecated: use addCor. Add correlations to parameter table
##'
##' Anything arguments are passed to `addCor()`. See `?addCor()`.
##' 
##' @param ... Passed to addCor
##' @export
## Deprecated with 0.1.9 Jan 2025

addOmegaCorr <- function(...){
    .Deprecated("addCor")
    addCor(...)

}
