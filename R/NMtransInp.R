##' translate the column names according to the $INPUT section of a control stream
##'
##' @param data the data to translate
##' @param file the list file or control stream
##' @param translate Do translation according to Nonmem code or not
##'     (default `TRUE`)? If not, an overview of column names in data
##'     and in Nonmem code is still returned with the data.
##' @param recover.cols recover columns that were not used in the
##'     NONMEM control stream? Default is TRUE. Can only be negative
##'     when translate=FALSE.
##' @param quiet Suppress warnings about data columns?
##' @details If `translate=FALSE`, data is returned with column names
##'     as in data file (not informed by the control stream `$INPUT`
##'     section). If `translate=TRUE`, `NMtransInp` renames and copies
##'     columns as specified in `$INPUT`. This means that
##'
##' @return data with column names translated as specified by nonmem
##'     control stream. Class same as for 'data' argument. Class
##'     data.table.
##' @import data.table
##' @keywords internal


## don't export. An internal function used by NMscanInput. 


NMtransInp <- function(data,file,lines,translate=TRUE,recover.cols=TRUE,quiet=FALSE){
    
#### Section start: Dummy variables, only not to get NOTE's in package checks ####

    . <- NULL
    .ESSBP. <- NULL
    col.data <- NULL
    compare <- NULL
    copy.1 <- NULL
    copy.2 <- NULL
    copy.left <- NULL
    copy.right <- NULL
    datafile <- NULL
    DATA <- NULL
    dup.dat <- NULL
    dup.inp <- NULL
    file.only <- NULL
    i.data <- NULL
    i.input <- NULL
    is.copy <- NULL
    INPUT <- NULL
    nonmem <- NULL
    result <- NULL
    ROW <- NULL
    str.input <- NULL

### Section end: Dummy variables, only not to get NOTE's in package checks ####
    
    ## stopifnot(is.data.table(data))
    if(!is.data.table(data)){
        data <- as.data.table(data)
    }
    cnames.data <- colnames(data)
    cnames.dup <- cnames.data[duplicated(cnames.data)]
    if(length(cnames.dup)){
        warning("Duplicate column names in input data file.")
    }
    nminfo.data.0 <- NMinfoDT(data)

    reserved.labels <- c(
        "ID", "L2", "DV", "MDV",
        "MRG_", "RAW_", "RPT_",
        "TIME", "EVID", "AMT", "RATE", "SS", "II", "ADDL", "CMT", "PCMT", "CALL", "CONT",
        "DATE", "DAT1", "DAT2", "DAT3", "L1")
    
#### this should be supported now
    ##    if( !translate && !recover.cols ) {messageWrap("recover.rows=FALSE is only allowed when translate=TRUE.",fun.msg=stop)}

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL
    lines0 <- lines
    
    ## According to NM manual IV-1, $INPUT and $INFILE are the same thing.
    lines <- NMreadSection(file,lines,section="INPUT",keep.name=FALSE,keep.comments=FALSE,clean.spaces=TRUE)
    if(is.null(lines)) {
        lines <- NMreadSection(file,lines0,section="INPT",keep.name=FALSE,keep.comments=FALSE,clean.spaces=TRUE)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INPT section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}

    ## drop comments
    
    ## names can be separated by , or " " or both. So one , between alphanumerics is replaced by a single space
    lines <- gsub("([[:alnum:]]) *, *([[:alnum:]])","\\1 \\2",lines)
    ## editors may include \t for a tidy view. Replace by space.
    lines <- gsub("\t"," ",lines)
    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)
    ## not sure what "DV =A", "DV= A" or "DV = A" would do but it may be valid, so reduce to DV=A
    line <- sub(" *= *","=",line)
    
    
### nms is the names of columns as in nonmem control stream
    nms <- strsplit(line," ")[[1]]
    nms <- cleanSpaces(nms,double=TRUE,lead=TRUE,trail=TRUE)
    nms0 <- nms

    dtcols <- TRUE
    if(dtcols){
        

        dt.cols <- dtFillCols(col.data=colnames(data),str.input=nms)
        dt.cols[!is.na(col.data),i.data:=.I]
        dt.cols[!is.na(str.input),i.input:=.I]
        ## dt.cols <- data.table(col.data=colnames(data))
        ## dt.cols[,str.input:=c(nms,rep(NA_character_,nrow(dt.cols)-length(nms)))][
        ##     ## result is exactly what?
        ##    ,result:=str.input]
        ## dt.cols[,result:=str.input]
        dt.cols[,result:=str.input]

        dt.cols[,drop:=FALSE]
        dt.cols[grepl("= *(DROP|SKIP)",str.input),drop:=TRUE]
        dt.cols[grepl("(DROP|SKIP) *=",str.input),drop:=TRUE]
        dt.cols[drop==TRUE,result:=sub("= *(DROP|SKIP)","",result)]
        dt.cols[drop==TRUE,result:=sub("(DROP|SKIP) *=","",result)]

        
#### issue: nms1 should be called nonmem. However, it should be reflective of whats available in Nonmem. 
        
### define pseudonyms
        ## should be called copy to align with NMdata documentation terminology
        ## For now, we just take the first name used in A=B labeling. 
        ## this has to come right after handling DROP
        dt.cols[,copy.left := NA_character_]
        dt.cols[,copy.right:= NA_character_]
        dt.cols[,copy := FALSE]
        dt.cols[grepl(".*=.*",result),copy := TRUE]
        expr.reserved <- paste0("(",paste(reserved.labels,collapse="|"),")")
        ## dt.cols[copy==TRUE&grepl(pattern=expr.reserved,result),copy1:=sub(paste0("\\(",expr.reserved,"\\)")),"\\1"]
        ## dt.cols[copy==TRUE&grepl(pattern=expr.reserved,result),reserved.found:=sub(paste0("\\(",expr.reserved,"\\)"),"\\1")]
        

        dt.cols[grepl(".*=.*",result),copy.left := sub("(.*)=(.*)","\\1",result)]
        dt.cols[grepl(".*=.*",result),copy.right:= sub("(.*)=(.*)","\\2",result)]
        dt.cols[,copy.1:=copy.right]
        dt.cols[,copy.2:=copy.left]
        dt.cols[copy==TRUE&grepl(pattern=expr.reserved,copy.left),(c("copy.1","copy.2")):=.(copy.left,copy.right)]
        
        dt.cols[copy==TRUE,result := copy.1]
        dt.cols[is.na(nonmem)&!is.na(copy.1),result:=copy.1]
        
        ## this is the names as read in nonmem. Keep them for reporting.
        ## dt.cols[,nonmem:=str.input]
        dt.cols[,nonmem:=result]
        ## second/right is "main"
        ## dt.cols[grepl(".*=.*",str.input),nonmem := sub("(.*) *= *(.*)","\\2",result)]
        ## dt.cols[copy==TRUE,nonmem := copy.1]
        ## dt.cols[is.na(nonmem)&!is.na(copy.right),nonmem:=copy.right]
        ## dt.cols[is.na(nonmem)&!is.na(copy.1),nonmem:=copy.1]

        ### drop means NONMEM doesnt see them
        dt.cols[drop==TRUE,nonmem:=NA]
### argument to exclude dropped variables from result?  
        
        if(translate){
            ## dt.cols[,result:=sub(".*=(.*)","\\1",result)]
            dt.cols[copy==TRUE,result:=copy.1]
        } else {
            dt.cols[,result:=col.data]
        }
        if(!recover.cols) {
            dt.cols[is.na(str.input),result:=NA]
        }

        dt.cols[,is.copy:=FALSE]
        ##  copy/pseudonyms/synononyms            
        dtc.copy <- dt.cols[!is.na(copy.1)&!is.na(copy.2)]
        if(translate && nrow(dtc.copy)){
            ##apply(dtc.copy,1,function(x)
            ## dtc.copy
            dt.cols <- rbind(dt.cols,
                             dtc.copy[,.(
                                 col.data,
                                 str.input,
                                 ## nonmem=copy.left,
                                 nonmem=copy.2,
                                 result=ifelse(rep(translate,.N),copy.2,NA),
                                 i.data,
                                 i.input,
                                 drop,
                                 copy.left,
                                 copy.right,
                                 copy.1,
                                 copy.2,
                                 is.copy=TRUE)]
                            ,fill=TRUE)
            
            
            ##data[,`:=`( get(dtc.copy$copy.right)=dtc.copy$copy.left)]
            ## for ( i in 1:nrow(dtc.copy)){
            ##     set(data,j=dtc.copy[i,copy.left] , value=data[,dtc.copy[i,col.data],with=FALSE])
            ## }
        }

        
        if(recover.cols){
            dt.cols[!is.na(col.data)&is.na(result),result:=col.data]
        }
        
        
### drop 
#### rename dropped variables if overwritten
        
        dt.cols[,ROW:=.I]
        dt.drop <- dt.cols[drop==TRUE]
        dt.drop
        dt.nodrop.dup <- merge(dt.drop[,.(result)],
                               dt.cols[!ROW%in%dt.drop[!is.na(str.input),ROW]]
                              ,
                               by="result")
        if(nrow(dt.nodrop.dup)){
            dt.drop.dup <- dt.drop[result%in%dt.nodrop.dup[,result]]
            for(r in 1:nrow(dt.drop.dup)){
                dt.cols[dt.drop.dup[r,ROW],result:=tmpcol(dt.cols[!is.na(str.input),result],base=paste0(result,"_DROP"))]
            }
        }

#### prioritize nonmem name even if dropped
        

### checks and modifications for unique naming
        if(translate){
            
            if(dt.cols[,any(is.na(col.data)&!is.na(str.input))]){
                if(!quiet){
                    messageWrap("More column names specified in Nonmem $INPUT than found in data file. The additional names have been disregarded.",fun.msg=warning)
                }
            }
            dt.cols[is.na(col.data)&!is.na(str.input),result:=NA_character_]
            
            
### same result, different str.input
            rows.dup <- unique(dt.cols,by="str.input")[
                duplicated(result)&
                !is.na(str.input)]

            if(nrow(rows.dup)){
                message("INPUT entries result in same variable names")
                dt.cols[i.data%in%rows.dup[,i.data],
                        result:=tmpcol(names=dt.cols[!is.na(str.input)&!is.na(result),result],base=result)]
            }

            ## one common counter to use in merges, like i.cols? 
            
### same result, has str.input. 
            rows.dup <- dt.cols[
                !is.na(str.input)][
                duplicated(result)]
            

            ## new name to be unique for those with str.input
            if(nrow(rows.dup)){
                message("INPUT entries result in same variable names")
                dt.cols[ROW%in%rows.dup[,ROW],
                        result:=tmpcol(names=dt.cols[!is.na(str.input)&!is.na(result),result],base=result)]
            }


### name in recovered data columns already used in INPUT?
            dt.cols[,dup.inp:=FALSE]
            dt.cols[is.na(str.input)&!is.na(col.data),dup.inp:=result%in%dt.cols[!is.na(str.input),result]]
            
##### something similar, but one row at a time
            ## dt.cols[dup.inp==TRUE,result:=tmpcol(base=paste0(result,"_FILE"))]
            
            if(dt.cols[,any(dup.inp)]){
                for(r in 1:nrow(dt.cols)){
                    if(dt.cols[r,dup.inp==TRUE]){
                        dt.cols[r,result:=tmpcol(names=dt.cols[1:(r-1)][!is.na(str.input)&!is.na(result),result],base=paste0(result,"_FILE"))]
                    }
                }
            }


### name repeated in recovered data columns
            dt.cols[,dup.dat:=FALSE]
            dt.cols[is.na(str.input)&!is.na(col.data),
                    dup.dat:=result%in%dt.cols[!is.na(str.input),result]]

            if(dt.cols[,any(dup.dat)]){
                for(r in 1:nrow(dt.cols)){
                    if(dt.cols[r,dup.dat==TRUE]){
                        dt.cols[r,result:=tmpcol(names=dt.cols[,result],base=paste0(result,"_FILE"))]
                    }
                }
            }

        }
        
        
        if(FALSE){
            
            nms2 <- cnames.input[-(1:length(nms))]
            if(!quiet && any(duplicated(nms))){
                messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first of the columns will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
            } 
            
            ## check for unique column names
            if(any(duplicated(cnames.input))) {
                nms2 <- cnames.input[-(1:length(nms))]
                if(!quiet && any(duplicated(nms))){
                    messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first of the columns will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
                } 
                if(!quiet && length(nms2)&&any(duplicated(nms2))){
                    messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first of the columns will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
                }
                nms.cross <- c(unique(nms),unique(nms2))
                if(!quiet && any(duplicated(nms.cross))){
                    
                    messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
                }
#### Reduce to unique column names
                data <- data[,unique(cnames.input),with=FALSE]
            }
        }
        
#### Reduce to unique column names
        
        ## data <- data[,dt.cols[!is.na(result),col.data],with=FALSE]
        if(translate){
            
            dt.cols[,file.only:=is.na(str.input)]
            ## dt.cols[,file.only:=is.na(str.input)]
            setorder(dt.cols,file.only,is.copy,i.input,i.data)
        }
        data <- data[,dt.cols[!is.na(result)&!is.na(i.data),i.data],with=FALSE]
        setnames(data,dt.cols[!is.na(result)&!is.na(i.data),result])

        dt.colnames <- dt.cols[,.(datafile=col.data,INPUT=str.input,nonmem,result)]
    }

    if(!dtcols){

### not using dt.cols from here
### this is to keep even dropped columns
        idx.drop <- grep("=(DROP|SKIP)",nms0)
        nms <- sub("(.*)=(DROP|SKIP)","\\1",nms)


        ## should be called copy to align with NMdata documentation terminology
        ## For now, we just take the first name used in A=B labeling. 
        renamed.from <- NULL
        renamed.to <- NULL
        renamed.from <- sub("(.*)=(.*)","\\1",nms[grepl(".*=.*",nms)])
        renamed.to <- sub("(.*)=(.*)","\\2",nms[grepl(".*=.*",nms)])
        ## left hand side is kept?
        nms <- sub(".*=(.*)","\\1",nms)

        ## this is the names as read. Keep them for reporting.
        nms1 <- nms

### cnames.input is the column names of the resulting data
        ## More column names can be specified in the nonmem control stream
        ## than actually found in the input data. We will simply disregard
        ## them.
        nminfo.data.0 <- NMinfoDT(data)
        cnames.input.0 <- copy(colnames(data))
        cnames.input <- copy(cnames.input.0)

### not necessary with dt.cols
        if(!recover.cols){
            data <- data[,1:length(nms)]
        }

        if(translate){
            if(length(nms)>length(cnames.input)){
                nms <- nms[1:length(cnames.input)]
                if(!quiet){
                    messageWrap("More column names specified in Nonmem $INPUT than found in data file. The additional names have been disregarded.",fun.msg=warning)
                }
            }
            cnames.input[1:length(nms)] <- nms
            
            if(!recover.cols){
                data <- data[,1:length(nms)]
                cnames.input <- cnames.input[1:length(nms)]
            }
            colnames(data) <- cnames.input
            
            ## add the synononyms
            if(!is.null(renamed.from)){
                
                data <- cbind(data,
                              setnames(data[,c(renamed.to),with=FALSE],old=renamed.to,new=renamed.from)
                              )
            }
            ## check for unique column names
            if(any(duplicated(cnames.input))) {
                nms2 <- cnames.input[-(1:length(nms))]
                if(!quiet && any(duplicated(nms))){
                    messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first of the columns will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
                } 
                if(!quiet && length(nms2)&&any(duplicated(nms2))){
                    messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first of the columns will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
                }
                nms.cross <- c(unique(nms),unique(nms2))
                if(!quiet && any(duplicated(nms.cross))){
                    
                    messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
                }
#### Reduce to unique column names
                
                data <- data[,unique(cnames.input),with=FALSE]
            }
        }
        
        length.max <- max(length(cnames.input.0), ## datafile
                          length(nms0),       ## DATA
                          length(nms1),       ## nonmem
                          length(colnames(data)) ## result
                          )
        dt.colnames <- data.table(datafile=c(cnames.input.0,rep(NA_character_,length.max-length(cnames.input.0))),
                                  ## DATA=c(nms0,rep(NA_character_,length.max-length(nms0))),
                                  "INPUT"=c(nms0,rep(NA_character_,length.max-length(nms0))),
                                  nonmem=c(nms1,rep(NA_character_,length.max-length(nms1))),
                                  ## result.all=c(colnames(data),rep(NA_character_,length.max-length(colnames(data))))
                                  result=c(colnames(data),rep(NA_character_,length.max-length(colnames(data))))
                                  )
    }


    ## compare: OK, diff, off
    dt.colnames[tolower(datafile)==tolower(INPUT),compare:="OK"]
    dt.colnames[tolower(datafile)!=tolower(INPUT),compare:="diff"]
    dt.colnames[compare=="diff"&tolower(INPUT)%in%tolower(datafile),compare:="off"]
    dt.colnames[,compare:=factor(compare,levels=c("OK","diff","off"))]


    writeNMinfo(data,nminfo.data.0,byRef=TRUE)
    writeNMinfo(data,list(input.colnames=dt.colnames),append=TRUE,byRef=TRUE)
    data
}
