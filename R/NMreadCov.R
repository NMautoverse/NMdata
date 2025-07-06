##' Read covariance matrix from `.cov` file
##'
##' @param file The ".cov" covariance Nonmem matrix file to read
##' @param auto.ext If `TRUE` (default) the extension will
##'     automatically be modified using `NMdataConf()$file.cov`. This
##'     means `file` can be the path to an input or output control
##'     stream, and `NMreadCov()` will still read the `.cov` file.
##' @param tableno The table number to read. The ".cov" file can
##'     contain multiple tables and will often do so if using SAEM/IMP
##'     methods. Default is "max" which means the last table is
##'     used. Alternative values are "min" and "all" or numeric
##'     values. If "all" or multiple numeric values are used, a list
##'     is returned. However, see `simplify` too.
##' @param simplify If `TRUE` (default) and only one table is returned
##'     (say using tableno="max") only that matrix is returned as a
##'     matrix object. If `FALSE` or multiple tables are returned, the
##'     result is a list.
##'
##' @return A matrix with covariance step from NONMEM or a list of
##'     such matrices (see `simplify`)
##' 
##' @export


NMreadCov <- function(file,auto.ext,tableno="max",simplify=TRUE) {

    if(missing(auto.ext) || is.null(auto.ext)) auto.ext <- TRUE
    fun.file.cov <- NMdataDecideOption("file.cov")
    if(auto.ext){
        file <- fun.file.cov(file)
    }

    if(!file.exists(file)){stop("file does not exist.")}

    if(is.null(tableno)) tableno <- "max"
    if( (is.character(tableno)&& !tableno%in%c("min","max","all") ) ||
        (is.numeric(tableno) && (tableno<=0 || tableno%%1!=0) )){
        stop("tableno must be either one of the character strings \"min\", \"max\", \"all\" or an integer greater than zero.")
    }

    res <- NMreadTabSlow(file=file,col.table.name=FALSE)

    if(tableno=="min"){
        tableno <- 1L
    }
    if(tableno=="max"){
        tableno <- length(res)
    }
    if(is.numeric(tableno)){
        ## res is a list
        res <- res[tableno]
    }
    
    res <- lapply(res,function(DT){

        res.mat <- DT[,setdiff(colnames(DT),c("TABLENO","table.name","NAME")),with=FALSE]
        res.mat <- as.matrix(res.mat)
        rownames(res.mat) <- DT$NAME
        res.mat
    })

    if(simplify && length(res)==1){
        res <- res[[1]]
    }
    res
}
