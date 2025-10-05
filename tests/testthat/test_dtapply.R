context("dtapply")

NMdataConf(reset=TRUE)

test_that("basic - return single value",{
    fileRef <- "testReference/dtapply_01.rds"

    res <- dtapply(1:3,function(x)paste0("x",x))
    
    expect_equal_to_reference(res,fileRef)

    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }
})

test_that("return dt",{
    fileRef <- "testReference/dtapply_02.rds"
    res <- dtapply(1:3,function(x)list(hey=paste(x)))
    expect_equal_to_reference(res,fileRef)

    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }
})


test_that("rename columns",{
    fileRef <- "testReference/dtapply_03.rds"
    res <- dtapply(1:3,function(x)paste0("x",x), value.names="val",element.name="x")
    expect_equal_to_reference(res,fileRef)
})

