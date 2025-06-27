context("listMissings")

test_that("basic",{
    NMdataConf(reset=TRUE)
    fileRef <- "testReference/listMissings_1.rds"

    pk <- readRDS(file="testData/data/xgxr2.rds")
    miss <- listMissings(pk,quiet=TRUE)

    expect_equal_to_reference(miss,fileRef,version=2)

})
