##' Writes a parameter values to a control stream
##'
##' Edit parameter values, fix/unfix them, or edit lower/upper bounds.
##' 
##' @param file.mod Path to control stream.
##' @param update If `TRUE` (default), the parameter values are
##'     updated based on the `.ext` file. The path to the `.ext` file
##'     can be specified with `file.ext` but that is normally not
##'     necessary.
##' @param file.ext Optionally provide the path to an `.ext` file. If
##'     not provided, the default is to replace the file name
##'     extention on `file.mod` with `.ext`. This is only used if
##'     `update=TRUE`.
##' @param ext An long-format parameter table as returned by
##'     `NMreadExt()`. Can contain multiple models if `file.mod` does
##'     not.
##' @param inits.tab A wide-format parameter table, well suited for
##'     customizing initial values, limits, and for fixing
##'     parameters. For multiple custom parameter specifications, this
##'     may be the most suitable argument.
##' @param values A list of lists. Each list specifies a parameter
##'     with named elements. Must be named by the parameter
##'     name. `lower`, `upper` and `fix` can be supplied to modify the
##'     parameter. See examples. Notice, you can use `...`
##'     instead. `values` may be easier for programming but other than
##'     that, most users will find `...` more intuitive.
##' @param newfile If provided, the results are written to this file
##'     as a new input control stream.
##' @param ... Parameter specifications. See examples,
##'
##' @details Limitations:
##' \itemize{
##'
##' \item `NMwriteInits()` can only update specifications of existing
##' parameters. It cannot insert new parameters.
##' 
##' \item lower, init, and upper must be on same line
##'
##' \item If using something like CL=(.1,4,15), two of those cannot be
##' on the same line
##' }
##' @return a control stream as lines in a character vector.
##' @examples
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr021.mod",package="NMdata") 
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


NMwriteInits <- function(file.mod,update=TRUE,file.ext=NULL,ext,inits.tab,values,newfile,...){
    
    . <- NULL
    blocksize <- NULL
    elemnum <- NULL
    elems.found <- NULL
    i <- NULL
    iblock <- NULL
    j <- NULL
    linenum <- NULL
    model <- NULL
    modified <- NULL
    parameter <- NULL
    parnum <- NULL
    par.type <- NULL
    type.elem <- NULL
    value.elem_init <- NULL
    value.elem_init_update <- NULL
    value.elem <- NULL
    value.elem_FIX <- NULL
    value <- NULL
    value.update <- NULL
    value.ext <- NULL
    value.initstab <- NULL

    V1 <- NULL


    fun.merge.tabs <- function(pars.l,tab.new,name.step){

        parameter <- NULL
        value.new <- NULL
        
        tab.new <- copy(tab.new)
        setDT(tab.new)

        ## we allow THETA(1) but the real parameter name is THETA1
        tab.new[,parameter:=sub("THETA\\(([0-9]+)\\)","THETA\\1",parameter)]
        tab.new <- addParType(tab.new)
        ## don´t use lower,upper,fix. Missing lower or upper will result in NA in table. Missing should mean don´t edit, not remove. But also, not sure we would use the tab.new interface to edit those. For now, those have to be edit trough the values interface
        
### todo check tab.new object
        ## tab.new must include a model variable
        ## max one of each par per model
        ## fix_col <- grep("^fix$", names(value.values), ignore.case = TRUE, value = TRUE)
        ## if (length(fix_col) > 1) {
        ##     stop("More than one variable name is \"fix\", independently of case. I Don\'t know what variable to use. You have to only provide one variable called \"fix\", ignoring case (i.e. \"FIX\", \"Fix\" also match).")
        ## }
        ## setnames(value.values, old = fix_col, new = "FIX")


        ## Target column names (case-insensitive)
        target_cols <- c("init", "lower", "upper", "FIX")

        ## Get all column names in the data
        colnames_data <- names(tab.new)

        ## Count matches per target
        n_matches <- sapply(target_cols, function(target) {
            sum(tolower(colnames_data) == tolower(target))
        })

        ## Check if any target is matched more than once
        if (any(n_matches > 1)) {
            stop("One or more target columns matched more than once, ignoring case:\n",
                 paste(target_cols[n_matches > 1], collapse = ", "))
        }

        ## Proceed only with those that match exactly once
        target_cols_to_rename <- target_cols[n_matches == 1]

        
        toConvert <- names(tab.new)[cleanSpaces(tolower(names(tab.new)))%in%tolower(target_cols)]
        tab.new[, (toConvert) := lapply(.SD,function(x) as.character(cleanSpaces(x))), .SDcols = toConvert]
        setnames(tab.new,toConvert,function(x)cleanSpaces(tolower(x)))
        setnames(tab.new,"fix","FIX",skip_absent = TRUE)
        toConvert[toConvert=="fix"] <- "FIX"
        
        inits.l <- melt(tab.new,measure.vars=toConvert,variable.name="type.elem",value.name="value.new")
        inits.l[type.elem=="FIX" & value.new=="0",value.new:=""]
        inits.l[type.elem=="FIX" & value.new=="1",value.new:=" FIX"]
        ##inits.l[type.elem=="FIX" & value.new=="1",value.new:=""]
        
        ## while "model" is needed, the value does not matter 
        ## if(!"model"%in%colnames(inits.l)) inits.l[,model:="inits1"]
        if(!"model"%in%colnames(inits.l)) {
            
            inits.l <- inits.l[,pars.l[,.(model=unique(model))],by=inits.l]
        }
        
        ## merge
        if(inits.l[,uniqueN(model)]>1){
            byCands <- c("parameter","par.type","i","j")
            cols.by <- c("model","type.elem",intersect(colnames(inits.l),byCands))

            ## repeat pars.l for each model found in inits.l
            pars.l <- egdt(pars.l[,!("model")],inits.l[,unique(model)])
            pars.l <- merge(pars.l,
                            inits.l[,c(cols.by,value.new)]
                           ,by=cols.by,all=TRUE)
        } 

        if(inits.l[,uniqueN(model)]==1){
            byCands <- c("model","parameter","par.type","i","j")
            cols.by <- c("type.elem",intersect(colnames(inits.l),byCands))

            pars.l <- merge(
                pars.l
               ,
                inits.l[,c(cols.by,"value.new"),with=FALSE]
               ,by=cols.by,all=TRUE)
        }
        
        ## update
        pars.l[!is.na(value.new)&value.new!="SAME",
               value.elem:=value.new
               ]
        setnames(pars.l,"value.new",paste0("value.",name.step))

        pars.l
    }
    
    
### maybe more than one model could be allowed. If not, NMsim will break all the time?    
    if(length(file.mod)>1) stop("`file.mod` points to more than one model. `NMwriteInits()` can only one model in `file.mod`.")

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

    if(missing(inits.tab)) inits.tab <- NULL
    if(!is.null(inits.tab)){
        warning("`inits.tab` argument experimental.")
    }

    if(is.null(file.ext)) file.ext <- file.mod
    lines.old <- readLines(file.mod,warn=FALSE)

    inits.orig <- NMreadInits(file=file.mod,return="all",as.fun="data.table")
    pars.l <- inits.orig$elements
    pars.l[,model:=fnExtension(basename(file.mod),"")]
    
    pars.l[type.elem=="FIX"&value.elem=="1",value.elem:=" FIX"]
    pars.l[type.elem=="FIX"&value.elem=="0",value.elem:=""]


############## write  parameter sections

    ## reduce lower, init and upper lines to just ll.init.upper lines
### for  this approach, dcast, then paste.ll...
    ## this is complicated. Better make paste function operate on long format.

######### Limitation: lower, init, and upper must be on same line



############ update paramters using .ext file
### update from ext. This methods drops all current values. Hence, it cannot be used for updating selected values.
    if(update){
        
        ext.new <- NMreadExt(file.ext,as.fun="data.table")

        pars.l <- mergeCheck(pars.l,
                             ext.new[,.(model,par.type,i,j,type.elem="init",
                                        value.update=as.character(value))]
                            ,by=c("model","par.type","i","j","type.elem"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)

        pars.l[!is.na(value.update)&value.update!="SAME",
               value.elem:=value.update
               ]
    }

    if(!is.null(ext)){
        ## if one file.mod, multiple models are allowed in ext
        if(pars.l[,uniqueN(model)]>1 && ext[,uniqueN(model)]>1  ){
            stop("Multiple models provided in file.mod and ext. This is not supported because it is Unclear how to merge them.")
        }

        if(ext[,uniqueN(model)]>1){
            pars.l <- egdt(pars.l[,setdiff(colnames(pars.l),"model"),with=FALSE],
                           ext[,.(model=unique(model))],
                           quiet=TRUE)

            pars.l <- mergeCheck(
                pars.l
               ,
                ext[,.(model,par.type,i,j,type.elem="init",
                       value.ext=as.character(value))]
               ,by=c("model","par.type","i","j","type.elem"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)
        } 

        if(ext[,uniqueN(model)]==1){
            pars.l <- mergeCheck(pars.l,
                                 ext[,.(par.type,i,j,type.elem="init",
                                        value.ext=as.character(value))]
                                ,by=c("par.type","i","j","type.elem"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)
        }

        pars.l[!is.na(value.ext)&value.ext!="SAME",
               value.elem:=value.ext
               ]
    }

    if(!is.null(inits.tab)){
        pars.l <- fun.merge.tabs(pars.l,inits.tab,name.step="initstab")
    }
    
### handle values. Move from NMwriteInitsOne.
    
    if(!is.null(values) && length(values)>0){
        
        values <- values[!names(values)%in%c("method")]
        names.values <- names(values)
        valuestab.list <- lapply(names.values,function(name){
            tab <- do.call(data.table,values[[name]])
            tab[,parameter:=name]
            tab
        })
        valuestab <- rbindlist(valuestab.list,fill=TRUE)

        pars.l <- fun.merge.tabs(pars.l,valuestab,name.step="values")

    }

    
############## write  parameter sections

    ## reduce lower, init and upper lines to just ll.init.upper lines
### for  this approach, dcast, then paste.ll...
    ## this is complicated. Better make paste function operate on long format.
    
### this is identifying positions of elements that were not in existing model. I don't know that we can handle that at the moment.
######### Limitation: lower, init, and upper must be on same line
    ## pars.l[,i2:=fifelse(is.na(linenum),max(i[!is.na(linenum)]),i),by=.(par.type,j)]
    ## pars.l[,j2:=fifelse(is.na(j),max(j[!is.na(j)]),j),by=.(par.type,i)]
    ## pars.l[,linenum:=uniquePresent(linenum),by=.(par.type,i2,j)]
    
    pars.l[,linenum:=uniquePresent(linenum),by=.(par.type,i,j)]

    pars.l[,parnum:=uniquePresent(parnum),by=.(par.type,i,j)]
### redefining parnumline to be within line
    ## pars.l[,parnumline:=1:.N,by=.(par.type,i,j)]
    pars.l[,iblock:=uniquePresent(iblock),by=.(par.type,i,j)]
    pars.l[,blocksize:=uniquePresent(blocksize),by=.(par.type,i,j)]
    
    inits.w <- dcastSe(pars.l,
                       l=intersect(c("model","par.type","linenum","parnum","i","j","iblock","blocksize"),colnames(pars.l)),
                       r="type.elem",
                       value.var=c("elemnum","value.elem"),funs.aggregate=min)

### the rest of the code is dependent on all of init, lower, and upper being available.
    cols.miss <- setdiff(outer(c("value.elem","elemnum"),c("init","lower","upper","FIX"),FUN=paste,sep="_"),colnames(inits.w))
    if(length(cols.miss)){
        inits.w[,(cols.miss):=NA_character_]
    }
    inits.w[is.na(value.elem_FIX),value.elem_FIX:=""]

    
    if("model"%in%colnames(inits.w)){
        all.models <- inits.w[,unique(model)]
        lines.new <- lapply(all.models,function(this.mod){
            ## lines.new <- lapply(split(inits.w,by="model"),function(dat){
            lines.res <- NMwriteInitsOne(lines=lines.old,
                                         inits.w=inits.w[model==this.mod],
                                         inits.orig=inits.orig,
                                         pars.l=pars.l[model==this.mod])
            lines.res
        })
        names(lines.new) <- all.models
        lines.new
    } else {
        lines.new <- NMwriteInitsOne(lines=lines.old,
                                     inits.w=inits.w,
                                     inits.orig=inits.orig,
                                     pars.l=pars.l)
    }

    if(!is.null(newfile)){
        ## if(length(lines.new)>1){
        ##     stop("cannot write files when number of resulting lines>1.")
        ## }
        writeTextFile(lines.new,newfile)
        return(invisible(lines.new))
    }

    lines.new
}
