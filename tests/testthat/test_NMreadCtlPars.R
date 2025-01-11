context("NMreadCtlPars")

test_that("basic",{

    fileRef <- "testReference/NMreadCtlPars_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
        
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    lines <- readLines(file.mod)
    res1 <- NMreadCtlPars(lines,section="theta")
    res1
    
    res2 <- NMreadCtlPars(lines,section="omega")
    res2

    res.all <- list(res1,res2)

    ## expect_equal_to_reference(res.all,fileRef)
    ## expect_equal(res1,res2)
        
})
