if(FALSE){
    ## read data

    file.mod <- "testData/nonmem/xgxr056.mod"
    inp <- NMscanInput(file.mod,translate=F,as.fun="data.table")

    inpres <- NMtransInp(inp,file=file.mod)

}
