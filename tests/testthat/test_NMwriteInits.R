context("NMwriteInits")

## ext <- NMreadExt(file.mod,as.fun="data.table")
## file.mod <- "/home/philip/wdirs/NMsim/devel/devel_writePars.mod"
## NMreadSection(file.mod,section="theta")



if(F){
    unloadNamespace("NMsim")
    unloadNamespace("NMdata")
    load_all("~/wdirs/NMdata")
    load_all("~/wdirs/NMsim")
}

test_that("Basic",{
    if(packageVersion("NMdata")>"0.1.8.921"){
        
        fileRef <- "testReference/NMwriteInits_01.rds"
        
        file.mod <- "testData/nonmem/xgxr033.mod"

        ## NMreadSection(file.mod,section="OMEGA")
        ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

        res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3))
        res1 <- NMreadSection(lines=res1,section="theta")

        NMreadSection(file.mod,section="theta")
        expect_equal_to_reference(res1,fileRef)


        ## unfix
        fileRef <- "testReference/NMwriteInits_02.rds"
        res1 <- NMwriteInits(file.mod,"OMEGA(1,1)"=list(fix=0))
        res1 <- NMreadSection(lines=res1,section="omega")

        ## The number of empty spaces seems to be inconsistent across platforms
        res1 <- gsub(" +"," ",res1)
        
        NMreadSection(file.mod,section="omega")
        expect_equal_to_reference(res1,fileRef)

        
#### fix a block
        fileRef <- "testReference/NMwriteInits_03.rds"
        res1 <- NMwriteInits(file.mod,"OMEGA(2,2)"=list(fix=1))
        res1 <- NMreadSection(lines=res1,section="omega")
        
        NMreadSection(file.mod,section="omega")

        ## The number of empty spaces seems to be inconsistent across platforms
        res1 <- gsub(" +"," ",res1)

        expect_equal_to_reference(res1,fileRef)

        
### modify omega in a block
        fileRef <- "testReference/NMwriteInits_04.rds"
        res1 <- NMwriteInits(file.mod,"OMEGA(2,2)"=list(init=1))
        res1 <- NMreadSection(lines=res1,section="omega")
        
        ## The number of empty spaces seems to be inconsistent across platforms
        res1 <- gsub(" +"," ",res1)

        NMreadSection(file.mod,section="omega")
        expect_equal_to_reference(res1,fileRef)

    }
})


test_that("comments on parameters",{
    if(packageVersion("NMdata")>"0.1.8.921"){
        fileRef <- "testReference/NMwriteInits_05.rds"
        file.mod <- "testData/nonmem/xgxr033.mod"
        ## NMreadSection(file.mod,section="THETA")
        res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3),"OMEGA(3,2)"=list(init=-4),"OMEGA(3,3)"=list(init=6),update=FALSE)
        
        ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

        res2 <- c(NMreadSection(lines=res1,section="theta"),
                  NMreadSection(lines=res1,section="omega")
                  )
        
        ## The number of empty spaces seems to be inconsistent across platforms
        res2 <- gsub(" +"," ",res2)

        expect_equal_to_reference(res2,fileRef)
    }
})


test_that("multiple named lists",{
    if(packageVersion("NMdata")>"0.1.8.921"){
        fileRef <- "testReference/NMwriteInits_06.rds"
        file.mod <- "testData/nonmem/xgxr033.mod"
        ## NMreadSection(file.mod,section="THETA")
        res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3,lower=.1,fix=1),"OMEGA(3,2)"=list(init=-4),"OMEGA(3,3)"=list(init=6),update=FALSE)
        
        ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

        res2 <- c(NMreadSection(lines=res1,section="theta"),
                  NMreadSection(lines=res1,section="omega")
                  )
        
        ## The number of empty spaces seems to be inconsistent across platforms
        res2 <- gsub(" +"," ",res2)

        expect_equal_to_reference(res2,fileRef)
    }
})
