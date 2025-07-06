
context("NMreadCov")

test_that("basic",{

    fileRef <- "testReference/NMreadCov_01.rds"
    file.cov <- "testData/nonmem/estim_debug.cov"

    res <- NMreadCov(file=file.cov)
    class(res)
    expect_equal_to_reference(res,fileRef)
})


test_that("A .cov with multiple tables",{

    fileRef <- "testReference/NMreadCov_02.rds"
    file.cov <- "testData/nonmem/xgxr032.cov"

    res1 <- NMreadCov(file=file.cov)
    
    expect_equal_to_reference(res1,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        res1
        ref
    }
    
})


test_that("A .cov with multiple tables - return all",{

    fileRef <- "testReference/NMreadCov_03.rds"
    file.cov <- "testData/nonmem/xgxr032.cov"

    res1 <- NMreadCov(file=file.cov,tableno="all")
    ## res2 <- NMreadCov2(file=file.cov)

    expect_equal_to_reference(res1,fileRef)
    
})
