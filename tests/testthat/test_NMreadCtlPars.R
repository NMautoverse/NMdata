context("NMreadCtlPars")

test_that("basic",{

    fileRef <- "testReference/NMreadCtlPars_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
        
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    lines <- readLines(file.mod)
    res1 <- NMreadCtlPars(lines)

    res1
    
    expect_equal_to_reference(res1,fileRef)

        
})



