
context("NMwriteSizes")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/NMreadSizes_01.rds"
    
    res <- NMreadSizes(file="testData/nonmem/xgxr051.mod")

    expect_equal_to_reference(res,fileRef)
    
})


test_that("from text lines",{
    fileRef <- "testReference/NMreadSizes_02.rds"

    sizes <- "$SIZES
PD  =2"

    res <- NMreadSizes(lines=sizes)

    expect_equal_to_reference(res,fileRef)
})
