##' @keywords internal

NMwriteInitsOne <- function(lines,inits.w,inits.orig,pars.l){

    . <- NULL
    elems.found <- NULL
    elemnum <- NULL
    elemnum_lower <- NULL
    elemnum_init <- NULL
    elemnum_upper <- NULL
    iblock <- NULL
    linenum <- NULL
    nchars.active <- NULL
    newtext <- NULL
    par.type <- NULL
    string.elem <- NULL
    text <- NULL
    text.after <- NULL
    text.before <- NULL
    type.elem <- NULL
    V1 <- NULL
    value.elem_FIX <- NULL
    value.elem_init <- NULL
    value.elem_lower <- NULL
    value.elem_upper <- NULL

    

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
    

    fun.update.ctl <- function(lines.old,section,dt.lines){
        par.type <- NULL
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

    lines <- fun.update.ctl(lines,section="THETA",dt.lines=lines.all.3)
    lines <- fun.update.ctl(lines,section="OMEGA",dt.lines=lines.all.3)
    lines <- fun.update.ctl(lines,section="SIGMA",dt.lines=lines.all.3)


    lines
}
