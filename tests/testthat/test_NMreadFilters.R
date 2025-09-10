context("NMreadFilters")

NMdataConf(reset=TRUE)
NMdataConf(as.fun="data.table")

test_that("basic",{
    fileRef <- "testReference/NMreadFilters_01.rds"

    res <- NMreadFilters("testData/nonmem/xgxr021.mod")

    expect_equal_to_reference(res,fileRef)

    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
        }
    
})
