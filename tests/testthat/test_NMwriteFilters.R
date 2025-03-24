context("NMwriteFilters")

test_that("basic",{

    NMdataConf(reset=TRUE)
    NMdataConf(as.fun="data.table")
    ## file.mod <- "/data/prod_vx548_lsr_phase2_analysis/trunk/analysis/PK_review/models/12746.mod"
    file.mod <- "testData/nonmem/xgxr032.mod"
    fileRef <- "testReference/NMwriteFilters_01.rds"

    filters <- NMreadFilters(file=file.mod)
    filters[cond=="FLAG.NE.0",cond:="FLAG.GT.10"]

    newlines <- NMwriteFilters(file=file.mod,filters=filters,write=FALSE)
    res <- NMreadSection(lines=newlines,section="data")

    expect_equal_to_reference(res,fileRef)

})
