context("NMtransInp")

fix.time <- function(x){
    meta.x <- attr(x,"NMdata")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$details$time.NMscanData <- NULL
    meta.x$details$file.lst <- NULL
    meta.x$details$file.mod <- NULL
    meta.x$details$file.input <- NULL
    meta.x$details$mtime.input <- NULL
    meta.x$details$mtime.lst <- NULL
    meta.x$details$mtime.mod <- NULL
    meta.x$datafile$path.csv <- NULL
    meta.x$datafile$path.rds <- NULL
    meta.x$datafile$path.fst <- NULL
    meta.x$tables$file <- NULL
    meta.x$tables$file.mtime <- NULL
    setattr(x,"NMdata",meta.x)
}


if(FALSE){
    ## read data

    ## file.mod <- "testData/nonmem/xgxr056.mod"
    file.mod <- "testData/nonmem/xgxr054.mod"
    inp <- NMscanInput(file.mod,translate=F,as.fun="data.table")
    names(inp)
    inpres <- 
        ## no recovering cols
        NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID NTIM",recover.cols=FALSE,translate=F)
}

test_that("basic",{

    fileRef <- "testReference/NMtransInp_01.rds"

    file.mod <- "testData/nonmem/xgxr054.mod"
    inp <- NMscanInput(file.mod,translate=F,as.fun="data.table")


    res <- list(
        ##  USUBJ comes last - ID prefered. OK.
        NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID NTIM",recover.cols=FALSE,translate=T)
       ,
        ##  USUBJ comes last - ID prefered (switching left and right in copy). OK.
        NMtransInp(data=inp,lines="$INPUT ROW ID=USUBJ NTIM",recover.cols=FALSE,translate=T)
       ,
        ## USUBJ is before recovered cols
        NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID NTIM",recover.cols=TRUE,translate=T)

    ,   

      ## AMT_FILE gets created. OK.
      NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID AMT",recover.cols=TRUE,translate=T)
    )

    
    res <- lapply(res,fix.time)
    ## attributes(res[[1]])[[3]]

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        ref[[1]]
        res[[1]]

        NMinfo(ref[[1]])
        NMinfo(res[[1]])
    }    
})

test_that("no trans",{
    file.mod <- "testData/nonmem/xgxr054.mod"
    inp <- NMscanInput(file.mod,translate=F,as.fun="data.table")

    res <- NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID NTIM",recover.cols=TRUE,translate=F)
    unNMdata(res)
    unNMdata(inp)
    expect_equal(res,inp)

})
