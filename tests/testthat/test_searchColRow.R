
library(data.table)
context("searchColRow")

test_that("basic",{
    
    fileRef <- "testReference/searchColRow_01.rds"

    file.mod <- "testData/nonmem/xgxr022.mod"
    inp <- NMscanInput(file.mod,as.fun="data.table")
    ## inp[,myrow:=.I]

    res1 <- searchColRow(file=file.mod,file.mod=file.mod,formats.read=c("rds","csv"),
                         dir.data=NULL,file.data=NULL,
                         translate.input=TRUE,col.id="ID",
                         args.fread=NMdataConf()$args.fread,
                         tab.row=inp
                         )

    expect_equal_to_reference(res1,fileRef)

})
