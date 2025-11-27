##' Overwrite values in one data.frame as available in another.
##'
##' Repair x with y
##' 
##' @param x The initial data.frame
##' @param y A data.frame to prioritize overc `x`.
##' @param by Columns to merge by
##' @param cols.coal Columns to overwrite values from `y` if available.
##'
##' @details Non-na values in y will be used o overwrite columns in x
##'     at the rows matched using `by` columns.
##'
##' Merges must be done using the same "by" columns for all rows. If
##' rows needs to be merged using varying by columns, the merges must
##' be done sequentially.
##' 
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

mergeCoal <- function(x,y,by,cols.coal,as.fun){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    x <- copy(as.data.table(x))
    y <- copy(as.data.table(y))

    col.row <- tmpcol(x,base="row")
    x[,(col.row):=.I]

    if(anyNA(y[,..by])){
        stop("missing values are not allowed in by columns of y.")
    }
    
    if(missing(cols.coal)) cols.coal <- NULL
    if(is.null(cols.coal)) cols.coal <- setdiff(intersect(colnames(x),colnames(y)),by)
    ## columnns in y that are not in x will be added (merged) using the same by columns
    cols.merge <- setdiff(colnames(y),colnames(x))

#### TODO: handle length(cols.coal)==0
    res <- x
    if(length(cols.coal)){
        x.l <- melt(x,id.vars=c(col.row,by),measure.vars=cols.coal,value.name="value.x")
        y.l <- melt(y,id.vars=by,measure.vars=cols.coal,value.name="value.y")
        
        xy.l <- mergeCheck(x.l,y.l,by=c(by,"variable"),all.x=TRUE,quiet=TRUE)
        xy.l[,value:=value.x]
        xy.l[!is.na(value.y),value:=value.y]
        
        xy <- dcastSe(xy.l,l=c(col.row,by),r="variable",value.var="value")
        res <- mergeCheck(
            x[,setdiff(colnames(x),cols.coal),with=FALSE]
           ,
            xy
           ,by=c(col.row,by)
           ,fun.na.by=NULL
           ,quiet=TRUE)
    }
    
    if(length(cols.merge)){
        res <- mergeCheck(res,y[,c(by,cols.merge),with=FALSE],by=by,all.x=T,quiet=TRUE)
    }

    res[,(col.row):=NULL]

    as.fun(res)
}
