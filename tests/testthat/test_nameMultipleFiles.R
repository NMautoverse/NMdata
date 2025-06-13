context("nameMultipleFiles")

test_that("All tests",{
    fileRef <- "testReference/nameMultipleFiles_01.rds"


    obj <- list(a=1,b=4)

    res <- nameMultipleFiles(fn="test.csv",list.obj=obj)

    expect_equal_to_reference(res,fileRef)

    })
