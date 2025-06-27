context("tryCatchAll")
NMdataConf(reset=TRUE)


test_that("Basic",{
    fileRef <- "testReference/tryCatchAll_01.rds"


    testfun <- function(x) {
        message("Starting function")
        if (x == 0) warning("x is zero")
        if (x < 0) stop("x is negative")
        message("Ending function")
        x
    }

    res1 <- tryCatchAll(testfun(1))  
    res1
    res1b <- tryCatchAll(testfun(1),message=FALSE)  
    res1b

    res2 <- tryCatchAll(testfun(0))  
    res2

    res3 <- tryCatchAll(testfun(-1)) 
    res3

    res.all <- list(res1,res1b,res2,res3)
    expect_equal_to_reference(res.all,fileRef)

})

