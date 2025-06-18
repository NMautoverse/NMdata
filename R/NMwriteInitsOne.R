##' @keywords internal

NMwriteInitsOne <- function(lines,inits.w,values,inits.orig,pars.l){

    . <- NULL
    elems.found <- NULL
    elemnum <- NULL
    elemnum_lower <- NULL
    elemnum_init <- NULL
    elemnum_upper <- NULL
    iblock <- NULL
    linenum <- NULL
    nchars.active <- NULL
    par.type <- NULL
    newtext <- NULL
    string.elem <- NULL
    type.elem <- NULL
    text <- NULL
    text.after <- NULL
    text.before <- NULL
    V1 <- NULL
    value.elem_init <- NULL
    value.elem_lower <- NULL
    value.elem_upper <- NULL
    value.elem_FIX <- NULL
    
### Implement changes to inits.w as requested in values
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
        ## names.vals <- names(value.values)
        ## names.vals[names.vals=="fix"] <- "FIX"
        ## names.vals[names.vals%in%c("init","lower","upper","FIX")] <- paste0("value.elem_",names.vals[names.vals%in%c("init","lower","upper","FIX")])
        ## names(value.values) <- names.vals

### any case-insensitive match to "fix" to "FIX". But no more than one match is allowed

        ## Assume value.values is a named list
        ## Example: value.values <- list(init=1, LOWER=2, extra=3)

        ## Target names to look for (case-insensitive)
        target_cols <- c("init", "lower", "upper", "FIX")

        ## Get current names of the list
        colnames_data <- names(value.values)

        ## Count matches per target (case-insensitive)
        n_matches <- sapply(target_cols, function(target) {
            sum(tolower(colnames_data) == tolower(target))
        })

        ## Stop if any target matches more than once
        if (any(n_matches > 1)) {
            stop("One or more target columns matched more than once:\n",
                 paste(target_cols[n_matches > 1], collapse = ", "))
        }

        ## Proceed with targets that are matched exactly once
        target_cols_to_rename <- target_cols[n_matches == 1]

        ## Get matched positions
        i_match <- match(tolower(target_cols_to_rename), tolower(colnames_data))

        ## Rename matched elements
        names(value.values)[i_match] <- paste0("value.elem.", colnames_data[i_match])


######### if value is data.table
        ## fix_col <- grep("^fix$", names(value.values), ignore.case = TRUE, value = TRUE)
        ## if (length(fix_col) > 1) {
        ##     stop("More than one variable name is \"fix\", independently of case. I Don\'t know what variable to use. You have to only provide one variable called \"fix\", ignoring case (i.e. \"FIX\", \"Fix\" also match).")
        ## }
        ## setnames(value.values, old = fix_col, new = "FIX")


        ## ## Target column names (case-insensitive)
        ## target_cols <- c("init", "lower", "upper", "FIX")

        ## ## Get all column names in the data
        ## colnames_data <- names(value.values)

        ## ## Count matches per target
        ## n_matches <- sapply(target_cols, function(target) {
        ##     sum(tolower(colnames_data) == tolower(target))
        ## })

        ## ## Check if any target is matched more than once
        ## if (any(n_matches > 1)) {
        ##     stop("One or more target columns matched more than once, ignoring case:\n",
        ##          paste(target_cols[n_matches > 1], collapse = ", "))
        ## }

        ## ## Proceed only with those that match exactly once
        ## target_cols_to_rename <- target_cols[n_matches == 1]

        ## ## Find matching positions
        ## i_match <- match(tolower(target_cols_to_rename), tolower(colnames_data))

        ## ## Rename matched columns
        ## setnames(value.values,
        ##          old = colnames_data[i_match],
        ##          new = paste0("value.elem.", colnames_data[i_match]))
        
        

        value$par.type <- par.type
        value$i <- i
        value$j <- j
        
        if(value$par.type=="THETA"){
            dt[par.type==value$par.type & i==value$i,
            (names(value.values)):=value.values]
        } else {
            dt[par.type==value$par.type & i==value$i & j==value$j,
            (names(value.values)):=value.values]
        }
        dt
    }

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

    
    if(missing(values)) values <- NULL

    if(length(values)){
        names.values <- names(values)
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
