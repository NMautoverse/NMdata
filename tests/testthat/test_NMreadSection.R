context("NMreadSection")




test_that("basic",{

    fileRef <- "testReference/NMreadSection_01.rds"
    
    section <- "input"
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod"
                        ,section=section
                         )
    
    expect_equal_to_reference(res,fileRef,version=2)

    if(F){
        ref <- readRDS(fileRef)
        ref
        res
    }
    
})

test_that("basic - data section",{

    fileRef <- "testReference/NMreadSection_02.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- "testData/nonmem/xgxr001.lst"

    res1 <- NMreadSection(file=file.lst,section="DATA")
    ## res1 <- NMreadSection(file=file.lst,section="DATA",debug=T)

    expect_equal_to_reference(res1,fileRef,version=2)
})


test_that("All sections",{

    fileRef <- "testReference/NMreadSection_03.rds"
    
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod")
    
    expect_equal_to_reference(res,fileRef,version=2)

    if(F){
        ref <- readRDS(fileRef)
        expect_equal(ref,
                     res)
        
        names(ref)
        names(res)
    }
    

})

test_that("Section not found",{

    section <- "simulation"
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod"
                        ,section=section
                         )
    
    expect_null(res)

})

test_that("HEADER",{

    fileRef <- "testReference/NMreadSection_04.rds"
    
    ## res <- NMreadSection(file="testData/nonmem/xgxr011.mod",section="header")
    res <- NMreadSection(file="testData/nonmem/xgxr011.mod")["HEADER"]
    ## res2 <- NMreadSection(file="testData/nonmem/xgxr011.mod",section="header",simplify=F)
    ## res <- NMreadSection(file="testData/nonmem/xgxr011.mod")[["HEADER"]]
    ## all <- NMreadSection(file="testData/nonmem/xgxr011.mod")
    ## all <- NMreadSection(file="testData/nonmem/xgxr011.mod",return="idx")
    ## all <- NMreadSection(file="testData/nonmem/xgxr011.mod")

    
    expect_equal_to_reference(res,fileRef,version=2)
    if(F){
        ref <- readRDS(fileRef)
        ref
        res
    }

})


test_that("simplify=FALSE",{

    fileRef <- "testReference/NMreadSection_05.rds"
    
    res.F <- NMreadSection(file="testData/nonmem/xgxr011.mod",section="DATA",simplify=F)
    res.T <- NMreadSection(file="testData/nonmem/xgxr011.mod",section="DATA",simplify=T)
    res <- list(res.F=res.F,
                res.T=res.T)
    expect_equal_to_reference(res,fileRef,version=2)
    if(F){
        ref <- readRDS(fileRef)
        ref
        res
    }

})





## all.text <- NMreadSection(file="testData/nonmem/xgxr011.mod")
## all.idx <- NMreadSection(file="testData/nonmem/xgxr011.mod",return="idx")

## all.text
## length(all.text)
## length(all.idx)
## NMreadSection(file="testData/nonmem/xgxr011.mod",section="header")
## NMreadSection(file="testData/nonmem/xgxr042.mod")
## NMreadSection(file="testData/nonmem/xgxr042.mod",keep.name=F)
