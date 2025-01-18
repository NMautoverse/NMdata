library(data.table)
context("addOmegaCorr")

test_that("basic",{

    fileRef <- "testReference/addOmegaCorr_01.rds"
    file.mod <- "testData/nonmem/xgxr022.mod"

    ext <- NMreadExt(file.mod)

    res1 <- addOmegaCorr(ext)

    res1
    expect_equal_to_reference(res1,fileRef)
        
})
