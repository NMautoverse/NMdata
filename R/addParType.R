##' Fill parameter names indexes in a data set
##'
##' Add par.type, i, j to a data.table that has parameter already
##'
##' @param pars Table of parameters to augment with additional columns
##' @param suffix Optional string to add to all new column
##'     names. Maybe except `i` and `j`.
##' @param add.idx Add `i` and `j`? Default is `TRUE` if no suffix is supplied, and `FALSE` if a suffix is specified.
##' @param overwrite Overwrite non-missing values? Default is `FALSE`.
##'
##' @details
##' `addParType()` fills in data sets of Nonmem parameter values to include the following variables (columns):
##' 
##' \itemize{
##' \item parameter: THETA1 , OMEGA(1,1), SIGMA(1,1), OBJ, SAEMOBJ
##' \item par.name: THETA(1), OMEGA(1,1), SIGMA(1,1), OBJ, SAEMOBJ
##' \item par.type THETA, OMEGA, SIGMA, OBJ
##' \item i: 1, 1, 1, NA, NA (No indexes for OBJ)
##' \item i: NA, 1, 1, NA, NA (j not defined for THETA)
##' }
##' 
##' As a last step, addParameter is called with overwrite=FALSE. This
##' fills parameter and par.name. Combined, if parameter is in pars, it is used. If not, par.type, i, and j are used.
##'
##' In the provided data set, parameter is allowed to have thetas as
##' THETA(1) (the par.name format). These will however be overwritten
##' with the described format above.

 
##' @keywords internal
addParType <- function(pars,suffix,add.idx,overwrite=FALSE){

    i <- NULL
    j <- NULL
    par.name <- NULL
    parameter <- NULL

    allpars <- c("THETA","OMEGA","SIGMA","THETAP","THETAPV","OMEGAP","OMEGAPD","SIGMAP","SIGMAPD")
    allpars.mat <- setdiff(allpars,"THETA")

    
    col.parameter <- "parameter"
    col.par.type <- "par.type"
    if(missing(suffix)) suffix <- NULL
    if(!is.null(suffix)){
        col.parameter <- paste(col.parameter,suffix,sep=".")
        col.par.type <- paste(col.par.type,suffix,sep=".")
    }

    if(missing(add.idx)) add.idx <- NULL
    if(is.null(add.idx)){
        add.idx <- is.null(suffix)
    }
    
    pars <- copy(pars)
    ## if THETA(N) is found, recode to THETAN
    pars[,parameter:=toupper(parameter)]
    pars[,parameter:=sub("THETA\\(([0-9]+)\\)","THETA\\1",parameter)]

    if(overwrite || !col.par.type%in%colnames(pars)){
        ## col.par.type
        pars[,(col.par.type):=NA_character_]
    }
    str.allpars <- paste0("^(",paste(allpars,collapse="|"),")")
    if(overwrite){
        pars[is.na(get(col.parameter))&grepl(str.allpars,get(col.parameter)),(col.par.type):=sub("^([A-Z]+).*","\\1",get(col.parameter))]
        pars[is.na(get(col.parameter))&get(col.parameter)%in%cc("OBJ","SAEMOBJ"),(col.par.type):="OBJ"]
    } else {
        pars[grepl(str.allpars,get(col.parameter)),(col.par.type):=sub("^([A-Z]+).*","\\1",get(col.parameter))]
        pars[get(col.parameter)%in%cc("OBJ","SAEMOBJ"),(col.par.type):="OBJ"]

    }
    
    ## i,j 
    if(add.idx){
        if(overwrite || !"i"%in%colnames(pars)){
            pars[get(col.par.type)=="THETA",i:=as.integer(sub("THETA([0-9][0-9]*)","\\1",get(col.parameter)))]

            pars[,row:=.I]
            pars[get(col.par.type)%in%allpars.mat,
                 i:=as.integer(sub(
                     pattern=sprintf("%s\\(([0-9]+)\\,([0-9]+)\\)",get(col.par.type)),
                     replacement="\\1",
                     x=get(col.parameter))),
                 by=row
                 ]
            pars[,row:=NULL]
        }

        
        
        if(overwrite || !"j"%in%colnames(pars) ){
            if(any(pars[,get(col.par.type)%in%allpars.mat])){
                
                pars[,row:=.I]
                pars[get(col.par.type)%in%allpars.mat,
                     j:=as.integer(sub(
                         pattern=sprintf("%s\\(([0-9]+)\\,([0-9]+)\\)",get(col.par.type)),
                         replacement="\\2",
                         x=get(col.parameter)
                     )),by=row]
                pars[,row:=NULL]
            }
        }
        
        ## cols <- cc(i,j)
        ## pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]
    }

    pars <- addParameter(pars,overwrite=FALSE)
    
    
    pars[]
}


##' add parameter based on par.type and i,j
##'
##' Columns filled or overwritten: parameter, par.name.
##'
##' @param pars Table of parameters to augment with additional columns.
##' @param overwrite Overwrite non-missing values? Default is `FALSE`.
##' @keywords internal
##' 
addParameter <- function(pars,overwrite=FALSE){
        
    par.name <- NULL
    par.type <- NULL
    parameter <- NULL
    i <- NULL
    j <- NULL

    allpars <- c("THETA","OMEGA","SIGMA","THETAP","THETAPV","OMEGAP","OMEGAPD","SIGMAP","SIGMAPD")
    allpars.mat <- setdiff(allpars,"THETA")
    col.parameter <- "parameter"
    col.par.type <- "par.type"

    pars <- copy(pars)
    pars[,par.type:=cleanSpaces(par.type)]
    pars[,par.type:=toupper(par.type)]

    if(overwrite || ! col.parameter%in%colnames(pars)){
        ## col.par.type
        pars[,(col.parameter):=NA_character_]
    }


    ##if(overwrite || !"parameter"%in%colnames(pars)){
    pars[is.na(get(col.parameter))&par.type=="THETA",parameter:=sprintf("%s%d",par.type,i)]
    pars[is.na(get(col.parameter))&par.type%in%allpars.mat,parameter:=sprintf("%s(%d,%d)",par.type,i,j)]
    ##}

    
    if(overwrite || ! "par.name"%in%colnames(pars)){
        ## col.par.type
        pars[,par.name:=NA_character_]
    }
    
    pars[is.na(par.name)&get(col.par.type)=="THETA",par.name:=sprintf("THETA(%s)",i)]
    pars[is.na(par.name),par.name:=get(col.parameter)]

    pars[]
}
