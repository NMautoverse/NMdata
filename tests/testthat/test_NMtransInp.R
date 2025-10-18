context("NMtransInp")

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

    expect_equal_to_reference(res,fileRef)

})

test_that("no trans",{
    file.mod <- "testData/nonmem/xgxr054.mod"
    inp <- NMscanInput(file.mod,translate=F,as.fun="data.table")

    res <- NMtransInp(data=inp,lines="$INPUT ROW USUBJ=ID NTIM",recover.cols=TRUE,translate=F)
    unNMdata(res)
    unNMdata(inp)
    expect_equal(res,inp)

})
