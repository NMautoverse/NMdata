context("NMwriteSizes")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/NMwriteSizes_01.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    res <- NMwriteSizes(file.mod,LTV=50,write=FALSE)

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        dt.res <- data.table(text=res)[,line:=.I]
        dt.ref <- data.table(text=ref)[,line:=.I]

        dt.all <- merge(dt.res,dt.ref,by="line")
        names(dt.all)
        dt.all[,text.x==text.y]

        opts <- options(widt=200)
        print(dt.all)
        options(opts)
    }
    
    
})


test_that("with exisiting $SIZES",{
    fileRef <- "testReference/NMwriteSizes_02.rds"

    file.mod <- "testData/nonmem/xgxr051.mod"

    res <- NMwriteSizes(file.mod,LTV=50,write=FALSE)
    res <- NMreadSection(lines=res,section="sizes")

    expect_equal_to_reference(res,fileRef)
    
    
})
