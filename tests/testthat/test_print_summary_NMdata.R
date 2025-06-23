context("print.summary_NMdata")
test_that("basic",{

    fileRef <- "testReference/print_summary_NMdata_01.rds"
    resRef <- if(file.exists(fileRef)) readRDS(fileRef) else NULL
    
    file.lst <- "testData/nonmem/xgxr001.lst"

    res.data <- NMscanData(file=file.lst,  order.columns = F, merge.by.row=FALSE, check.time = FALSE,quiet=T)
    sum.res <- summary(
        res.data
    )

    sum.res
    
    res <- capture.output(
        print(sum.res),type="message"
    )
    
   # res
    
    
    expect_equal_to_reference(res,fileRef,version=2)

    if(F){
        ref <- readRDS(fileRef)
        ref
        res
    }
})


## NMscanTables(file.lst,meta.only = T)
