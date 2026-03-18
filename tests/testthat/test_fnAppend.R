context("fnAppend")

test_that("basic",{

    fileRef <- "testReference/fnAppend_1.rds"
    
    res1 <- list(
        ## numeric
        fnAppend("feww.csv",1)
       ,
        ## character
        fnAppend("feww.csv","one")
       ,
        ## 
        fnAppend("feww.csv","")
    )

    expect_equal_to_reference(res1,fileRef)
    
})


test_that("empty string does nothing",{

    str1 <- "fe.ef"
    str2 <- fnAppend(str1,"")
    expect_identical(str1,str2)

})

test_that("multiple strings to append",{

    fileRef <- "testReference/fnAppend_02.rds"
    
    res1 <- c(
        fnAppend("NMsim.rds",c("simname","sim2"))
       ,
        ## the strings must both be appended to both fn's.
        fnAppend(c("NMsim.rds","NMsim2.rds"),c("simname","sim2"))    
    )

    expect_equal_to_reference(res1,fileRef)

    if(F){
        readRDS(fileRef)
    }
    
})



test_that("skip directory double dots",{
    
    fileRef <- "testReference/fnAppend_03.rds"
    
    expect_error(
        fnAppend("fe/../egef","hmm")
    )
    ## should also return error:
    expect_error(
        fnAppend("egef","hmm")
    )

    res <- c(fnAppend("fe/../egef","hmm",allow.noext = TRUE),
             fnAppend("egef","hmm",allow.noext = TRUE))

    expect_equal_to_reference(res,fileRef)
    
})


test_that("prepend basic",{

    ## fileRef <- "testReference/fnAppend_1.rds"

    ## numeric    
    res1 <- fnAppend("few.csv",1,position="prepend")

    expect_equal(res1,"1_few.csv")    
})


test_that("appending to strings like geee..",{

    ### there was an issue appending to strings like "geee.."
    fileRef <- "testReference/fnAppend_04.rds"

    res1 <- list(
        fnAppend("few.","ggg",allow.noext=T)
       ,fnAppend("few..","ggg",allow.noext=T)
       ,fnAppend("hey/few..","ggg",allow.noext=T)
       ,fnAppend("hey/few..mod","ggg",allow.noext=T)
       ,fnAppend("hey/few...mod","ggg",allow.noext=T)
       ,fnAppend("few...mod","ggg",allow.noext=T)
       ,fnAppend("xgxr021_NMreadSim_path...mod","ggg",allow.noext=T)
       ,fnAppend("xgxr021_NMreadSim_path...mod","ggg",allow.noext=F)
    )
    
    expect_equal_to_reference(res1,fileRef)
})

test_that("no file name stem",{
    res <- fnAppend(".jpg", "afile")
    expect_equal(res, "afile.jpg")
})

test_that("Attach multiple separate strings",{
    res <- fnAppend("file.jpg", c("a","b"))
    res <- fnAppend("file.jpg", c("a","b"),collapse=NULL)
    expect_equal(res, c("file_a.jpg","file_b.jpg"))



})

test_that("Attach multiple separate strings",{

    res <- fnAppend(c("file1.jpg","file2.jpg"), c("a","b"),collapse=NULL)
   expect_equal(res,c("file1_a.jpg", "file1_b.jpg", "file2_a.jpg", "file2_b.jpg"))  
})
