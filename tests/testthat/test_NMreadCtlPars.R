context("NMreadCtlPars")

test_that("basic",{

    fileRef <- "testReference/NMreadCtlPars_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    lines <- readLines(file.mod)
    res1 <- NMreadCtlPars(lines=lines,return="all")

    res1
    
    expect_equal_to_reference(res1,fileRef)

    
})

test_that("with OMEGA block",{
    NMdataConf(reset=T)

    fileRef <- "testReference/NMreadCtlPars_02.rds"

    file.mod <- "testData/nonmem/xgxr133.mod"
    res1 <- NMreadCtlPars(file=file.mod,return="all")

    expect_equal_to_reference(res1,fileRef)

})

test_that("OMEGA SAME",{

    fileRef <- "testReference/NMreadCtlPars_03.rds"
    
    text <- c("
$THETA
(0,0.1) ; THE1      - 30) 1st theta
 (0,4.2) ; THE2        - 31) 2nd theta
$OMEGA  0.08   ;    IIV.TH1  ; 1  ;IIV
 $OMEGA  BLOCK(1)
 0.547465  ; IOV.TH1  ; 2 ;IOV
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME")

    res1 <- NMreadCtlPars(lines=text,return="all")

    expect_equal_to_reference(res1,fileRef)
})
