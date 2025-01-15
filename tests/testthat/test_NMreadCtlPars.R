context("NMreadCtlPars")

test_that("basic",{

    fileRef <- "testReference/NMreadCtlPars_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
        
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    lines <- readLines(file.mod)
    res1 <- NMreadCtlPars(lines=lines)

    res1
    
    expect_equal_to_reference(res1,fileRef)

        
})

file.mod <- "/data/home/philipde/wdirs/NMdata/inst/examples/nonmem/xgxr133.mod"
file.mod <- "/data/home/philipde/wdirs/NMsim/tests/testthat/testData/nonmem/xgxr021com.mod"
res1 <- NMreadCtlPars(file=file.mod,return="all")
res1
ext <- NMreadExt(file.mod)
NMreadSection(file.mod,section="OMEGA")
res1$elements

head(ext)
dcast(res1$elements,par.type+i+j~type.elem,value.var="value.elem")
setDT(res1)
res1$elements[,.(par.type,parameter,par.name,i,j,value,lower,upper,FIX)]
