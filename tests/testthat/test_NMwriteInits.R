context("NMwriteInits")

## ext <- NMreadExt(file.mod,as.fun="data.table")
## file.mod <- "/home/philip/wdirs/NMsim/devel/devel_writePars.mod"
## NMreadSection(file.mod,section="theta")



if(F){
    ## unloadNamespace("NMsim")
    ## unloadNamespace("NMdata")
    ## load_all("~/wdirs/NMdata")
    ## load_all("~/wdirs/NMsim")
}

test_that("Basic",{

    
    fileRef <- "testReference/NMwriteInits_01.rds"
    
    file.mod <- "testData/nonmem/xgxr033.mod"

    ## NMreadSection(file.mod,section="OMEGA")
    ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

    res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3),update=FALSE)
    NMreadSection(file=file.mod,section="theta")
    res1 <- NMreadSection(lines=res1,section="theta")

    NMreadSection(file.mod,section="theta")
    expect_equal_to_reference(res1,fileRef)


    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res1
    }

})

test_that("Update + value",{

    
    fileRef <- "testReference/NMwriteInits_01b.rds"
    
    file.mod <- "testData/nonmem/xgxr033.mod"

    ## NMreadSection(file.mod,section="OMEGA")
    ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

    res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3),update=TRUE)
    NMreadSection(file=file.mod,section="theta")
    res1 <- NMreadSection(lines=res1,section="theta")

    NMreadSection(file.mod,section="theta")
    expect_equal_to_reference(res1,fileRef)


    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res1
    }

})


test_that("unfix",{
    
    fileRef <- "testReference/NMwriteInits_02.rds"
    file.mod <- "testData/nonmem/xgxr031.mod"
    NMreadSection(file=file.mod,section="omega")
    
    res1 <- NMwriteInits(file.mod,"OMEGA(1,1)"=list(fix=0),update=F)
    res1 <- NMreadSection(lines=res1,section="omega")
    ## res1
    ## The number of empty spaces seems to be inconsistent across platforms
    res1 <- gsub(" +"," ",res1)

    expect_equal_to_reference(res1,fileRef)


    if(FALSE){
        ref <- readRDS(fileRef)
        NMreadSection(file.mod,section="omega")
        ref
        res1
    }

})

#### 
test_that("fix a block",{
    fileRef <- "testReference/NMwriteInits_03.rds"
    file.mod <- "testData/nonmem/xgxr031.mod"
    ## NMsim
    ## file.mod <- "~/wdirs/NMsim/tests/testthat/testData/nonmem/xgxr033.mod"
    file.mod <- "testData/nonmem/xgxr053.mod"
    
    res1 <- NMwriteInits(file.mod,"OMEGA(2,2)"=list(fix=1),update=FALSE)
    res1 <- NMreadSection(lines=res1,section="omega")


    ## The number of empty spaces seems to be inconsistent across platforms
    res1 <- gsub(" +"," ",res1)

    expect_equal_to_reference(res1,fileRef)

    if(FALSE){
        NMreadSection(file.mod,section="omega")
        ref <- readRDS(fileRef)
        ref
        res1
    }
    
})

test_that(" modify omega in a block",{
    fileRef <- "testReference/NMwriteInits_04.rds"

    file.mod <- "testData/nonmem/xgxr053.mod"
    res1 <- NMwriteInits(file.mod,"OMEGA(2,2)"=list(init=1))
    res1 <- NMreadSection(lines=res1,section="omega")

    ## The number of empty spaces seems to be inconsistent across platforms
    res1 <- gsub(" +"," ",res1)

    NMreadSection(file.mod,section="omega")
    expect_equal_to_reference(res1,fileRef)

    if(FALSE){
        NMreadSection(file.mod,section="omega")
        NMreadExt(file.mod,as.fun="data.table")[par.type=="OMEGA",.(parameter,est)]
        ref <- readRDS(fileRef)
        ref
        res1
    }

    
})



test_that("comments on parameters",{
    fileRef <- "testReference/NMwriteInits_05.rds"
    ## file.mod <- "testData/nonmem/xgxr033.mod"
    
    file.mod <- "testData/nonmem/xgxr053.mod"
    
    ## NMreadSection(file.mod,section="THETA")
    res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3),"OMEGA(3,2)"=list(init=-4),"OMEGA(3,3)"=list(init=6),update=FALSE)


    
    ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

    res2 <- c(NMreadSection(lines=res1,section="theta"),
              NMreadSection(lines=res1,section="omega")
              )
    
    ## The number of empty spaces seems to be inconsistent across platforms
    res2 <- gsub(" +"," ",res2)

    expect_equal_to_reference(res2,fileRef)

    if(FALSE){
        NMreadSection(file.mod,section="theta")
        NMreadSection(file.mod,section="omega")
        ref <- readRDS(fileRef)
        ref
        res2
    }

    
})


test_that("multiple named lists",{
    fileRef <- "testReference/NMwriteInits_06.rds"
    file.mod <- "testData/nonmem/xgxr053.mod"
    ## NMreadSection(file.mod,section="THETA")
    res1 <- NMwriteInits(file.mod,"THETA(1)"=list(init=3,lower=.1,fix=1),"OMEGA(3,2)"=list(init=-4),"OMEGA(3,3)"=list(init=6),update=FALSE)
    
    ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

    res2 <- c(NMreadSection(lines=res1,section="theta"),
              NMreadSection(lines=res1,section="omega")
              )
    
    ## The number of empty spaces seems to be inconsistent across platforms
    res2 <- gsub(" +"," ",res2)

    expect_equal_to_reference(res2,fileRef)

    if(FALSE){
        NMreadSection(file.mod,section="theta")
        NMreadSection(file.mod,section="omega")
        ref <- readRDS(fileRef)
        ref
        res2
    }


    
})



test_that("An ext object",{
    if(packageVersion("NMdata")>"0.2.0.901"){
        fileRef <- "testReference/NMwriteInits_07.rds"
        file.mod <- "testData/nonmem/xgxr033.mod"
        ## readLines(file.mod)
        ext <- NMreadExt(file.mod,as.fun="data.table")
        ext <- rbind(ext,
                     transform(ext,model="mod2",value=value*1.3,est=est*1.3)
                     )
        ## NMreadSection(file.mod,section="THETA")
        res1 <- expect_warning(
            NMwriteInits(file.mod,ext=ext,update=FALSE)
        )
        
        ## The number of empty spaces seems to be inconsistent across platforms
        res1 <- lapply(res1,function(r){
            r <- gsub(" +"," ",r)
            r <- r[grepl("\\$THETA",r)|grepl("\\$OMEGA",r)|grepl("\\$SIGMA",r)]
            r
        })
        ext[,.(model,parameter,value)]
        res1
        expect_equal_to_reference(res1,fileRef)

        if(F){
            ref <- readRDS(fileRef)            
            ref
            res1
        }
    }
})


test_that("an inits.tab object",{

### "init", "lower", "upper", "FIX")
    fileRef <- "testReference/NMwriteInits_08.rds"
    file.mod <- "testData/nonmem/xgxr033.mod"

    inits.tab <- fread(text="parameter,init
THETA(1),3")
    
    ##options(warn=2)

    res1 <- expect_warning(
        NMwriteInits(file.mod,update=FALSE,inits.tab=inits.tab)
    )

})
