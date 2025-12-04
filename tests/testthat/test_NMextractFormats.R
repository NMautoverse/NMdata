context("NMextractFormats")

test_that("delim includes -",{

    fileRef <- "testReference/NMextractFormats_01.rds"

    NMdataConf(reset=T)
    NMdataConf(as.fun="data.table")


    text <- c("
;; format: -%symbol ;-%trans; %idx; %panel; %label;%unit
$THETA
(0, 4.4)	; -CL		; -none		
$OMEGA
0.15		; -IIV.KA	; -lognormal
$OMEGA BLOCK(1)
0.15		; -IOV1.KA	; -lognormal
")


    lines <- strsplit(text,split="\n")[[1]]
    NMdata:::as.NMctl(lines,lines=T)
    res <- NMextractFormats(ctl=NMdata:::as.NMctl(lines,lines=T))

    expect_equal_to_reference(res,fileRef)

})

test_that("format or parstext",{

    text1 <- c("
;; format: -%symbol ;-%trans; %idx; %panel; %label;%unit
$THETA
(0, 4.4)	; -CL		; -none		
$OMEGA
0.15		; -IIV.KA	; -lognormal
$OMEGA BLOCK(1)
0.15		; -IOV1.KA	; -lognormal
")

text2 <- sub("format","ParsText",text1)

    lines <- strsplit(text1,split="\n")[[1]]
    NMdata:::as.NMctl(lines,lines=T)
    res1 <- NMextractFormats(ctl=NMdata:::as.NMctl(lines,lines=T))

    lines <- strsplit(text2,split="\n")[[1]]
    NMdata:::as.NMctl(lines,lines=T)
    res2 <- NMextractFormats(ctl=NMdata:::as.NMctl(lines,lines=T))

expect_equal(res1,res2)
})
