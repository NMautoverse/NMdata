context("NMreadInits")

test_that("basic",{

    fileRef <- "testReference/NMreadInits_01.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    
    NMdataConf(reset=T)
    ## NMdataConf(as.fun="data.table")

    lines <- readLines(file.mod)
    res1 <- NMreadInits(lines=lines,return="all")

cat(paste(res1$lines$text.before,collapse=":"),"\n")
cat(paste(readRDS(fileRef)$lines$text.before,collapse=":"),"\n")

    expect_equal_to_reference(res1,fileRef)

    
})

test_that("with OMEGA block",{
    NMdataConf(reset=T)

    fileRef <- "testReference/NMreadInits_02.rds"

    file.mod <- "testData/nonmem/xgxr133.mod"
    res1 <- NMreadInits(file=file.mod,return="all")

    expect_equal_to_reference(res1,fileRef)

})

test_that("OMEGA SAME",{
    NMdataConf(reset=T)
    fileRef <- "testReference/NMreadInits_03.rds"
    
    text <- c("
$THETA
(0,0.1) ; THE1      - 30) 1st theta
 (0,4.2) ; THE2        - 31) 2nd theta
$OMEGA  0.08   ;    IIV.TH1  ; 1  ;IIV
 $OMEGA  BLOCK(1)
 0.547465  ; IOV.TH1  ; 2 ;IOV
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME")

    res1 <- NMreadInits(lines=text,return="all")

    expect_equal_to_reference(res1,fileRef)
})
