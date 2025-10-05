context("lapplydt")

NMdataConf(reset=TRUE)

test_that("basic",{
    fileRef <- "testReference/lapplydt_01.rds"
    dat <- readRDS("testData/data/xgxr2.rds")
    dat
    res <- lapplydt(dat,by="ID",fun=dim)
    
    expect_equal_to_reference(res,fileRef)
})



test_that("custom function",{
    fileRef <- "testReference/lapplydt_02.rds"
    dat <- readRDS("testData/data/xgxr2.rds")

    res <- lapplydt(dat,by="ID",fun=function(x)x[1])
    expect_equal_to_reference(res,fileRef)
})
