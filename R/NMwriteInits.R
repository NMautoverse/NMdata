##' Writes a parameter values to a control stream
##'
##' Edit parameter values, fix/unfix them, or edit lower/upper bounds.
##' 
##' @param file.mod Path to control stream.
##' @param update If `TRUE` (default), the parameter values are
##'     updated based on the `.ext` file.
##' @param file.ext Optionally provide the path to an `.ext` file. If
##'     not provided, the default is to replace the file name
##'     extention on `file.mod` with `.ext`. This is only used if
##'     `update=TRUE`.
##' @param ext Not implemented.
##' @param values A list of lists. Each list specifies a parameter
##'     with named elements. Must be named by the parameter name. `lower`,
##'     `upper` and `fix` can be supplied to modify the parameter. See
##'     examples. Notice, you can use `...` instead. `values` may be easier for programming but other than that, most users will find `...` more intuitive.
##' @param newfile If provided, the results are written to this file
##'     as a new input control stream.
##' @param ... Parameter specifications. See examples,
##'
##' @details Limitations:
##' \itemize{
##' \item lower, init, and upper must be on same line
##' \item If using something like CL=(.1,4,15), two of those cannot be on the same line
##' }
##' @return a control stream as lines in a character vector.
##' @examples
##' ## Requires NMdata 0.1.9
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr021.mod",package="NMsim") 
##' NMwriteInits(file.mod,
##' values=list( "theta(2)"=list(init=1.4),
##'              "THETA(3)"=list(FIX=1),
##'              "omega(2,2)"=list(init=0.1))
##' )
##' NMwriteInits(file.mod,
##'   "theta(2)"=list(init=1.4),
##'   "THETA(3)"=list(FIX=1),
##'   "omega(2,2)"=list(init=0.1)
##' )
##' }
##' @import data.table
##' @export


NMwriteInits <- function(file.mod,update=TRUE,file.ext=NULL,ext,values,newfile,...){

    . <- NULL
    elemnum <- NULL
    elems.found <- NULL
    i <- NULL
    iblock <- NULL
    j <- NULL
    model <- NULL
    modified <- NULL
    par.type <- NULL
    type.elem <- NULL
    value.elem_init <- NULL
    value.elem_init_update <- NULL
    value.elem <- NULL
    value.elem_FIX <- NULL
    value <- NULL
    V1 <- NULL


    
    if(missing(values)) values <- NULL
    dots <- list(...)
    values <- append(values,dots)
    
    
    if(any(!tolower(unlist(sapply(values,names)))%in%c("init","lower","upper","fix"))){
        stop("`values` must be a list of named lists.
  Example: values=list('theta(1)'=list(init=2))
  The allowed elements in each list is 'init', 'lower', 'upper', and 'fix'.")
    }

    if(missing(newfile)) newfile <- NULL
    
#### 
    if(missing(ext)) ext <- NULL

    if(!is.null(ext)){
        warning("`ext` argument experimental.")
    }

    
    if(update || !is.null(ext)){
        replace.inits <- TRUE
    }
    
    inits.orig <- NMreadInits(file=file.mod,return="all",as.fun="data.table")
    pars.l <- inits.orig$elements
    
    
    if(is.null(file.ext)) file.ext <- file.mod
    lines.old <- readLines(file.mod,warn=FALSE)
    
############## write  parameter sections

    ## reduce lower, init and upper lines to just ll.init.upper lines
### for  this approach, dcast, then paste.ll...
    ## this is complicated. Better make paste function operate on long format.
    
######### Limitation: lower, init, and upper must be on same line
    pars.l[type.elem=="FIX",value.elem:=fifelse(value.elem=="1"," FIX","")]
    inits.w <- dcast(
        pars.l[type.elem%in%c("lower","init","upper","FIX")]
       ,par.type+linenum+parnum+i+j+iblock+blocksize~type.elem,value.var=c("elemnum","value.elem"),funs.aggregate=min)

### the rest of the code is dependent on all of init, lower, and upper being available.
    cols.miss <- setdiff(outer(c("value.elem","elemnum"),c("init","lower","upper","FIX"),FUN=paste,sep="_"),colnames(inits.w))
    if(length(cols.miss)){
        inits.w[,(cols.miss):=NA_character_]
    }
    inits.w[is.na(value.elem_FIX),value.elem_FIX:=""]

    
    
############ update paramters using .ext file
    ## I don´t think modified is used anymore
    ## inits.w[,modified:=0]
### update from ext. This methods drops all current values. Hence, it cannot be used for updating selected values.
    if(update){
        ext.new <- NMreadExt(file.ext,as.fun="data.table")

        inits.w <- mergeCheck(inits.w,ext.new[,.(par.type,i,j,value.elem_init_update=as.character(value))],by=c("par.type","i","j"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)
        inits.w[value.elem_init!="SAME",value.elem_init:=value.elem_init_update]
        inits.w[,value.elem_init_update:=NULL]

    }


    if(!is.null(ext)){
        ## don´t use lower,upper,fix. Missing lower or upper will result in NA in table. Missing should mean don´t edit, not remove. But also, not sure we would use the ext interface to edit those. Fo now, those have to be edit trough the values interface
        
### todo check ext object
        ## ext must include a model variable
        ## max one of each par per model
        
        inits.w <- merge(inits.w,ext[,.(model,par.type,i,j,value.elem_init_update=as.character(value))],by=c("par.type","i","j"),all.x=TRUE)
        
        inits.w[value.elem_init!="SAME"&!is.na(value.elem_init_update),value.elem_init:=value.elem_init_update]
        inits.w[,value.elem_init_update:=NULL]

    }
    
    if("model"%in%colnames(inits.w)){
        lines.new <- lapply(split(inits.w,by="model"),function(dat){
            lines.new <- NMwriteInitsOne(lines.old,dat,values=values,inits.orig=inits.orig,pars.l)
            lines.new
        })
    } else {
        lines.new <- NMwriteInitsOne(lines.old,inits.w,values=values,inits.orig=inits.orig,pars.l)
    }

    if(!is.null(newfile)){
        if(length(lines.new)>1){
            stop("cannot write files when number of resulting lines>1.")
        }
        writeTextFile(lines.new,newfile)
        return(invisible(lines.new))
    }
    
    lines.new
    
}
