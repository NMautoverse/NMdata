context("splitFields")

library(data.table)
data.table::setDTthreads(1)

test_that("basic",{

    fileRef <- "testReference/splitFields_01.rds"

    res <- splitFields("%init;[%num2];%symbol")
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
    
})



context("NMreadParText")

library(data.table)
data.table::setDTthreads(1)

readRef <- FALSE


test_that("muref SAEM",{

    fileRef <- "testReference/NMreadParText_02.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    
    
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod,format="%init;%symbol")
    
    expect_equal_to_reference(res,fileRef)
    
    if(F){
        NMreadSection(file.mod,section="theta")
        res
        readRDS(fileRef)
    }
    
})


test_that("merge with NMreadExt output",{

    fileRef <- "testReference/NMreadParText_03.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")
    
    res <- NMreadParsText(file.mod,format="%init;%symbol")
    
    res <- mergeCheck(
        res,
        NMreadExt(file.mod)[,.(parameter,est)],
        by="parameter")

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

    
})


test_that("complex delimiters",{

    fileRef <- "testReference/NMreadParText_04.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA
; missing field
$THETA  (3)             ;  [] ;LTVV2
; missing end field
$THETA  (1)             ;[3]
; extra delim
$THETA  (4)             ;[4] ;LTVV3 ;
; missing end field but has delim
$THETA  (-1)             ; [5] ;

$OMEGA 0 FIX 

$SIGMA 0 FIX

")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num2];%symbol")

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

    
})



test_that("No SIGMA",{

    fileRef <- "testReference/NMreadParText_05.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA

$OMEGA 0 FIX 

")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num2];%symbol")

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})


test_that("Complex OMEGA",{

    fileRef <- "testReference/NMreadParText_06.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA (mL/h)
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
  0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
  0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
  0.38  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
$SIGMA 1
")
    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,format="%init;[%num];%symbol (%unit)",
                          format.omega="%init  ; %symbol     ; %num ; %type   ; %label ; %unit",field.idx="num"
                          ## ,use.idx=T
                          )


    expect_equal_to_reference(res,fileRef)

    
    if(F){
        res
        readRDS(fileRef)
    }

})


text <- c("
; matches format
$THETA  (.1)             ;[1]; LTVKA (mL/h)
$OMEGA  BLOCK(3)
0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-

0.024  0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
  0.2 0.2   0.38  ; IIV.CL.V3.cov  ; 3 ;IIV     ;Covariance of BSV on CL and V3;-
$OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
$SIGMA 1
")
lines <- strsplit(text,split="\n")[[1]]

res <- NMreadParsText(lines=lines,format="%init;[%num];%symbol (%unit)",
                      format.omega="%init  ; %symbol     ; %num ; %type   ; %label ; %unit",field.idx="num"
                      ## ,use.idx=T
                      )


test_that("OMEGA SAME",{

### BLOCK SAME are being skipped
    
    fileRef <- "testReference/NMreadParText_07.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    text <- c("
$THETA
(0,0.1) ; THE1      - 30) 1st theta
 (0,4.2) ; THE2        - 31) 2nd theta
$OMEGA  0.08   ;    IIV.TH1  ; 1  ;IIV
 $OMEGA  BLOCK(1)
 0.547465  ; IOV.TH1  ; 2 ;IOV
$OMEGA  BLOCK(1) SAME; IOV.TH1 ; 3; IOV2
$OMEGA  BLOCK SAME")

    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,
                          format="%init;%symbol - %idx) %label",
                          format.omega="%init; %symbol  ; %idx  ; %label "
                          )

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})


test_that("muref SAEM - format.omega=NULL",{

    fileRef <- "testReference/NMreadParText_08.rds"
    file.mod <- "testData/nonmem/xgxr032.mod"
    
    
    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")

    res1 <- NMreadParsText(file.mod,format="%init;%symbol")    
    res2 <- NMreadParsText(file.mod,format="%init;%symbol",format.omega=NULL)
    
    expect_equal(res1,res2)
    
})

### redundant, see "OMEGA SAME"
## test_that("Occassion variability",{
##     fileRef <- "testReference/NMreadParText_09.rds"
##     file.mod <- "testData/nonmem/xgxr044.mod"
##     NMdataConf(reset=T)
##     NMdataConf(as.fun="data.table")

##     res1 <- NMreadParsText(file.mod,format="%init;%symbol")
##     expect_equal_to_reference(res1,fileRef)
##     if(F){
##         res1
##         readRDS(fileRef)
##     }
## })


test_that("OMEGA SAME with linebreks",{

### BLOCK SAME are being skipped
    
    fileRef <- "testReference/NMreadParText_09.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")


    text <- c("
$THETA
(0, 4.4)	; CL		; none		; 1	; struct	; Clearance	; L/h
$OMEGA
0.15		; IIV.KA	; lognormal 	; 7	; IIV		; Between-subject variability on KA 	; -
$OMEGA BLOCK(1)
0.15		; IOV1.KA	; lognormal 	; 8	; IOV		; Between-occasion variability on KA 	; -
$OMEGA BLOCK(1)
SAME ; IOV2.KA	; lognormal 	; 9	; IOV		; Between-occasion variability on KA 	; -
$OMEGA BLOCK(1) SAME ; IOV3.KA	; lognormal 	; 10	; IOV		; Between-occasion variability on KA 	; -
$OMEGA BLOCK(1)
0.2		; IIV.D1	; lognormal 	; 11	; IIV		; Between-subject variability on D1 	; -
$OMEGA BLOCK(1)
0.3		; IOV1.D1	; lognormal 	; 12	; IOV		; Between-occasion variability on D1 	; -
$OMEGA BLOCK(1) SAME ; IOV2.D1	; lognormal 	; 13	; IOV		; Between-occasion variability on D1 	; -
$OMEGA BLOCK(1) SAME ; IOV3.D1	; lognormal 	; 14	; IOV		; Between-occasion variability on D1 	; -
")


    lines <- strsplit(text,split="\n")[[1]]

    res <- NMreadParsText(lines=lines,
                          format="%init;%symbol ;%trans; %idx; %panel; %label;%unit"
                          )

    expect_equal_to_reference(res,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        ref
        res
        
    }


})
