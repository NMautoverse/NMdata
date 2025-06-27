##' Add blocking info to parameter set
##' @param pars The parameter, as returned by `NMreadExt()`
##' @param col.model Name of the model name column.
##' @keywords internal

addBlocks <- function(pars,col.model="model"){

    . <- NULL
    blocksize <- NULL
    est <- NULL
    i <- NULL
    iblock <- NULL
    imin <- NULL
    j <- NULL
    model <- NULL
    parameter <- NULL
    par.type <- NULL
    value <- NULL

    
    pars <- addParType(pars)

    setcolorder(pars,intersect(c(col.model,"TABLENO","NMREP","table.step","par.type","parameter","par.name","i","j","FIX","value", "cond","eigCor",   "partLik",   "se", "seStdDevCor", "stdDevCor", "termStat"),colnames(pars)))
    
    pars <- pars[!parameter%in%c("SAEMOBJ","OBJ")]
    
### this setorder call doesnt work - unsure why
    ## setorder(pars,match(par.type,c("THETA","OMEGA","SIGMA")),i,j)
    pars <- pars[order(get(col.model),match(par.type,c("THETA","OMEGA","SIGMA")),i,j)]
    ## est is just a copy of value for backward compatibility
    pars[,est:=value]

    if("iblock"%in%colnames(pars)){
        pars[,iblock:=NULL]
    }
    if("blocksize"%in%colnames(pars)){
        pars[,blocksize:=NULL]
    }
    
### add OMEGA block information based on off diagonal values
    tab.i <- rbind(pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=i,j=j,value)],
                   pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=j,j=i,value)])[
        ## include i==j so that if an OMEGA is fixed to zero it is still assigned an iblock
        i==j|abs(value)>1e-9,.(iblock=min(i,j)),by=.(par.type,i)]
    tab.i[,blocksize:=.N,by=.(par.type,iblock)]

    pars <- mergeCheck(pars,tab.i,by=cc(par.type,i),all.x=T,quiet=TRUE)


### 
    pars[abs(i-j)>(blocksize-1),(c("iblock","blocksize")):=list(NA,NA)]
    pars[!is.na(iblock),imin:=min(i),by=.(iblock)]
    pars[j<imin,(c("iblock","blocksize")):=list(NA,NA)]
    pars[,imin:=NULL]

    
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&is.na(iblock),iblock:=i]
    pars[par.type%in%c("OMEGA","SIGMA")&i==j&iblock==i&is.na(blocksize),blocksize:=1]
### done add OMEGA/SIGMA blocks

    pars
    
}
