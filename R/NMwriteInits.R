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
    elemnum_lower <- NULL
    elemnum_init <- NULL
    elemnum_upper <- NULL
    elems.found <- NULL
    i <- NULL
    iblock <- NULL
    j <- NULL
    linenum <- NULL
    modified <- NULL
    newtext <- NULL
    nchars.active <- NULL
    par.type <- NULL
    string.elem <- NULL
    text <- NULL
    text.after <- NULL
    text.before <- NULL
    type.elem <- NULL
    value.elem_lower <- NULL
    value.elem_init <- NULL
    value.elem_upper <- NULL
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
        warning("`ext` argument not implemented. Ignored.")
    }

    
    if(update || !is.null(ext)){
        replace.inits <- TRUE
    }
    
    inits.orig <- NMreadInits(file=file.mod,return="all",as.fun="data.table")
    pars.l <- inits.orig$elements
    
    if(is.null(file.ext)) file.ext <- file.mod


    
############## write  parameter sections
    ## need to write line by line. All elements in a line written one at a time
    
    paste.ll.init.ul <- function(lower,init,upper,FIX){
        
        res <- NULL
        
        if(any(is.na(init))) stop("An initial value must be provided")
        if(any(!is.na(upper)&is.na(lower))) stop("if upper limit is provided, lower limit must also be provided.")
        dt <- data.table(lower=lower,init=init,upper=upper)[,row:=.I]
        dt[init=="SAME",res:=init]
        dt[init!="SAME",res:=paste0("(",paste(setdiff(c(lower,init,upper),NA),collapse=","),")",FIX),by=row]
        dt[init!="SAME"&is.na(lower)&is.na(upper),res:=paste0(init,FIX),by=row]
        dt$res
    }
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
    ##    inits.w[,fix:=ifelse(FIX=="1","FIX","")]
    inits.w[is.na(value.elem_FIX),value.elem_FIX:=""]

    
    
############ update paramters
    inits.w[,modified:=0]
### update from ext
    if(update){
        
        ext.new <- NMreadExt(file.ext,as.fun="data.table")

        inits.w <- mergeCheck(inits.w[,-("value.elem_init")],ext.new[,.(par.type,i,j,value.elem_init=as.character(value))],by=c("par.type","i","j"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)
    }

    
### Implement changes as requested in values
    fun.update.vals <- function(dt,value,name){
        par.type <- NULL
        text <- NULL
        
        names(value) <- tolower(names(value))

        name <- toupper(name)
        name <- gsub(" +","",name)
        par.type <- sub("^([A-Z]+)\\(.*","\\1",name)

        if(par.type=="THETA"){
            i <- as.integer(sub(paste0(par.type,"\\(([0-9]+)\\)"),"\\1",name))
            j <- NA
        }

        if(par.type%in%c("OMEGA","SIGMA")){
            i <- as.integer(sub(paste0(par.type,"\\(([0-9]+),([0-9]+)\\)"),"\\1",name))
            j <- as.integer(sub(paste0(par.type,"\\(([0-9]+),([0-9]+)\\)"),"\\2",name))
        }

        
        if("fix" %in% names(value)) {
            if(value$fix) {
                value$fix <- " FIX"
            } else {
                value$fix <- ""
            }
        }


        
        
        ## value.values <- value[setdiff(names(value),c("par.type","i","j"))]
        value.values <- value
        names.vals <- names(value.values)
        names.vals[names.vals=="fix"] <- "FIX"
        names.vals[names.vals%in%c("init","lower","upper","FIX")] <- paste0("value.elem_",names.vals[names.vals%in%c("init","lower","upper","FIX")])
        names(value.values) <- names.vals
        ## make sure FIX is "" or " FIX"

        value$par.type <- par.type
        value$i <- i
        value$j <- j
        
        if(value$par.type=="THETA"){
            dt[par.type==value$par.type & i==value$i, (names(value.values)):=value.values]
        } else {
            dt[par.type==value$par.type & i==value$i & j==value$j, (names(value.values)):=value.values]
        }
        dt
    }

    
    names.values <- names(values)
    if(length(values)){
        for(I in 1:length(values)){
            inits.w <- fun.update.vals(inits.w,values[[I]],names.values[I])
        }
    }

    
    
######### format paramters for ctl
    inits.w[,type.elem:="ll.init.ul"]
    inits.w[,row:=1:.N]

    
    
    inits.w[,string.elem:=paste.ll.init.ul(value.elem_lower,value.elem_init,value.elem_upper,value.elem_FIX),by=row]
    inits.w[,elemnum:=min(elemnum_lower,elemnum_init,elemnum_upper,na.rm=TRUE),by=row]

    cnames.common <- intersect(colnames(pars.l),colnames(inits.w))
    elems.all <- rbind(
        pars.l[!type.elem%in%c("lower","init","upper","FIX")][,cnames.common,with=FALSE]
       ,
        inits.w[,cnames.common,with=FALSE]
    )

    elems.all <- elems.all[order(par.type,linenum,elemnum)]
    elems.all[,row:=.I]
    ## idx.update <- elems.all[par.type%in%c("OMEGA","SIGMA"), row[1], by = .(par.type,iblock)][,V1]
    idx.update <- elems.all[, row[1], by = .(par.type,iblock)][,V1]
    elems.all[idx.update, string.elem := paste(paste0("$",par.type),string.elem)]

    ## lines.all should also include empty lines and before and after text

    lines.all <- elems.all[,.(text=paste(string.elem,collapse=" ")),keyby=.(par.type,linenum)]

    mod.lines <- inits.orig$lines
    
    
    lines.all.2 <- elems.all[,.(newtext=paste(string.elem,collapse=" ")),keyby=.(par.type,linenum)]
    lines.all.2[,elems.found:=TRUE]
##### this is the new total lines obj
    lines.all.3 <- mergeCheck(mod.lines,lines.all.2,by=c("par.type","linenum"),all.x=TRUE,quiet=TRUE)
##### correct elems.found=NA to FALSE
    lines.all.3[is.na(elems.found),elems.found:=FALSE]
#### update newtext for lines without elements. This will only work if text was read with keep.name=FALSE
    lines.all.3[elems.found==FALSE,newtext:=sub(pattern=paste0("^ *\\$ *",par.type),replacement="",x=text,ignore.case=TRUE),by=.(par.type,linenum)]

    

    lines.all.3[elems.found==TRUE&!is.na(text.before),newtext:=paste(
                                                          sub(pattern=paste0("\\$ *",par.type),"",text.before,ignore.case=TRUE)
                                                         ,newtext
                                                      ),by=.(par.type,linenum)]

    
    ## number of characters to reserve for before+newtext
    lines.all.3[elems.found==TRUE,nchars.active:=max(nchar(newtext))+1,by="par.type"]
    lines.all.3[,row:=.I]
    
    lines.all.3[elems.found==TRUE,newtext:=paste0(newtext,paste(rep(" ",nchars.active-nchar(newtext)),collapse="")),by=row]
    
    lines.all.3[elems.found==TRUE&!is.na(text.after),newtext:=paste(
                                                         newtext,
                                                         paste0(";",text.after)
                                                     ),by=.(par.type,linenum)]
    lines.all.3[,text:=newtext]
    

    lines.new <- readLines(file.mod,warn=FALSE)

    fun.update.ctl <- function(lines.old,section,dt.lines){
        text <- NULL
        newsection <- dt.lines[par.type==section,text]
        if(length(newsection)==0) return(lines.old)
        
        NMwriteSectionOne(lines=lines.old,
                          section=section,
                          newlines=newsection,
                          location="replace",
                          quiet=TRUE,
                          backup=FALSE)
    }

    lines.new <- fun.update.ctl(lines.new,section="THETA",dt.lines=lines.all.3)
    lines.new <- fun.update.ctl(lines.new,section="OMEGA",dt.lines=lines.all.3)
    lines.new <- fun.update.ctl(lines.new,section="SIGMA",dt.lines=lines.all.3)
    

    if(!is.null(newfile)){
        writeTextFile(lines.new,newfile)
        return(invisible(lines.new))
    }
    
    lines.new
    
}
