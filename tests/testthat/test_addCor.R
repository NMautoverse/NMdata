library(data.table)
context("addCor")


test_that("basic",{

    fileRef <- "testReference/addCor_01.rds"
    file.mod <- "testData/nonmem/xgxr022.mod"

    ext <- NMreadExt(file.mod)

    res1 <- addCor(ext)

    ## expect_snapshot_value(res1, style = "serialize")
    expect_equal_to_reference(res1, fileRef)

    if(F){
        ref <- readRDS(fileRef)
        compareCols(ref,res1)
        ref
        res1

    }

    
})
