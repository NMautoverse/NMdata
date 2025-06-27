context("getLines")

NMdataConf(reset=TRUE)

test_that("basic",{

    fileRef <- "testReference/getLines_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    
    res.file <- getLines(file=file.mod)
    expect_equal_to_reference(res.file,fileRef)

    res.lines <- getLines(lines=res.file)

    expect_equal(res.file,res.lines)
})
