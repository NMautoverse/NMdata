##' Overwrite values in one data.frame as available in another.
##'
##' Repair data.frame x with values present in data.frame y. Columns to match by
##' must be provided.
##' 
##' @param x The initial data.frame
##' @param y A data.frame to prioritize overc `x`.
##' @param by Columns to merge by. A character vector, with names to columns present in both `x` and `y`. At least one of by and cols.merge must be provided.
##' @param cols.coal Columns to overwrite values from `y` if
##'     available. cols.coal must be present in y and may be present in x.
##' @param add.new If columns in y are ne to x, merge them in? Default is to do
##'     so.
##' @param as.fun Pass a function (say tibble::as_tibble) in as.fun to convert
##'     to something else. If data.tables are wanted, use
##'     as.fun="data.table". The default is to return data as a
##'     data.frame. Modify the defaul using `NMdataConf()`.
##'
##' @details Non-na values in y will be used o overwrite columns in x
##'     at the rows matched using `by` columns.
##'
##' Merges must be done using the same "by" columns for all rows. If
##' rows needs to be merged using varying by columns, the merges must
##' be done sequentially.
##'
##' Will try to guess by and cols.coal. 
##' @examples
##' library(data.table)
##' x <- data.table(idx=1:3,a=paste0("xa",1:3),b=paste0("xb",1:3))
##' y <- data.table(idx=1:2,a=c("ya1",NA),b=c(NA,"yb2"))
##' mergeCoal(x,y,by="idx")
##' ## multiple rows in x matched by one row in y
##' x <- data.table(idx=1:3,grp=c(1,1,2),a=paste0("xa",1:3),b=paste0("xb",1:3))
##' y <- data.table(grp=1,a="y1")
##' mergeCoal(x,y,by="grp")
##' ## new column (c in y, not in x)
##' x <- data.table(idx=1:3,a=paste0("xa",1:3),b=paste0("xb",1:3))
##' y <- data.table(idx=1:2,a=c("ya1",NA),b=c(NA,"yb2"),c=1)
##' mergeCoal(x,y,by="idx")
##' @export

mergeCoal <- function(x,y,by,cols.coal,add.new=TRUE,as.fun){

#### Dummy variables, only not to get NOTE's in pacakge checks ####

    ..by <- NULL
    value <- NULL
    value.x <- NULL
    value.y <- NULL

### End: Dummy variables, only not to get NOTE's in pacakge checks ###



    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(by)) by <- NULL
    ## cols.coal allowed to be FALSE which means include none.
### cols.coal as NULL means use default, derived from by
    if(missing(cols.coal)) cols.coal <- NULL
    
    if(is.null(by)&&is.null(cols.coal)){
        by <- setdiff(intersect(colnames(x),colnames(y)))
        message("Neither of `by` and `cols.coal` provided. Using all common columns in x and y as by variables.")
    }
    
    x <- copy(as.data.table(x))
    y <- copy(as.data.table(y))

    ## row counter column in x gets a unique column name, not present in y.
    col.row.x <- tmpcol(names=unique(c(colnames(x),colnames(y))),base="xrow")
    x[,(col.row.x):=.I]

    if(!is.null(cols.coal)){
        if(!all(cols.coal%in%colnames(y))){
            stop("cols.coal must be a character vector of column names found in y")
        }
    }
    if(!is.null(by)){
        if(!all(by%in%colnames(x)) || !all(by%in%colnames(y))){
            stop("by must be a character vector of column names found in both x and y")
        }
    }
    
    if(is.null(cols.coal) && !is.null(by)) cols.coal <- setdiff(intersect(colnames(x),colnames(y)),by)
    if(isFALSE(cols.coal)) {
        ## now setting cols.coal to NULL. After this point NULL means nothing will be done.
        if(is.null(by)) by <- setdiff(intersect(colnames(x),colnames(y)),cols.coal)
        cols.coal <- NULL
    } else {
        if(!is.null(cols.coal) && is.null(by)) by <- setdiff(intersect(colnames(x),colnames(y)),cols.coal)
    }
    

    if(is.null(by)){
        stop("No by variables found. By must be supplied and point to columns present in both x and y.")
    }
    if(anyNA(y[,..by])){
        stop("missing values are not allowed in by columns of y.")
    }


    ## columnns in y that are not in x will be added (merged) using the same by columns
    if(add.new){
        cols.merge <- setdiff(colnames(y),colnames(x))
    } else {
        cols.merge <- setdiff(cols.coal,colnames(x))
    }
    cols.coal <- setdiff(cols.coal,cols.merge)

#### iteratively applying coal, and merging, allowing for including any or none of those steps
    res <- x
    if(length(cols.coal)){
        x.l <- melt(x,id.vars=c(col.row.x,by),measure.vars=cols.coal,value.name="value.x")
        y.l <- melt(y,id.vars=by,measure.vars=cols.coal,value.name="value.y")
        
        xy.l <- mergeCheck(x.l,y.l,by=c(by,"variable"),all.x=TRUE,quiet=TRUE)
        xy.l[,value:=value.x]
        xy.l[!is.na(value.y),value:=value.y]
        
        xy <- dcastSe(xy.l,l=c(col.row.x,by),r="variable",value.var="value")
        res <- mergeCheck(
            x[,setdiff(colnames(x),cols.coal),with=FALSE]
           ,
            xy
           ,by=c(col.row.x,by)
           ,fun.na.by=NULL
           ,quiet=TRUE)
    }
    
    if(length(cols.merge)){
        res <- mergeCheck(res,y[,c(by,cols.merge),with=FALSE],by=by,all.x=T,quiet=TRUE)
    }

    res[,(col.row.x):=NULL]

    as.fun(res)
    }
