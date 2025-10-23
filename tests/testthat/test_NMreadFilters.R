context("NMreadFilters")

NMdataConf(reset=TRUE)
NMdataConf(as.fun="data.table")

test_that("basic",{
    fileRef <- "testReference/NMreadFilters_01.rds"

    res <- NMreadFilters("testData/nonmem/xgxr021.mod")

    expect_equal_to_reference(res,fileRef)

    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }
    
})

test_that("no filters",{
    fileRef <- "testReference/NMreadFilters_02.rds"

    ## res <- NMreadFilters(lines="$DATA     ../data/xgxr2.csv")
    res <- NMreadFilters(lines="$DATA     ../data/xgxr2.csv",filters.only=FALSE)

    expect_equal_to_reference(res,fileRef)

    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }
    
})

test_that("Nested - multiple filters in one set of parentheses",{

    lines1 <- "$DATA      xgxr2.csv IGNORE=@ IGNORE=(FLAG.NE.0) IGNORE(DOSE.LT.30)"
    res1 <- NMreadFilters(lines=lines1)
    lines2 <- "$DATA      xgxr2.csv IGNORE=@ IGNORE=(FLAG.NE.0,DOSE.LT.30)"
    res2 <- NMreadFilters(lines=lines2)

    expect_equal(res1,res2)

})
