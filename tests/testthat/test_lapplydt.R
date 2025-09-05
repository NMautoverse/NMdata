
if(FALSE){
    context("dtapply")


    test_that("basic",{
        fileRef <- "testReference/lapplydt_01.rds"
        dat <- readRDS("testData/data/xgxr2.rds")
        dat
        res <- lapplydt(dat,by="ID",fun=dim)

        res
    })

}
