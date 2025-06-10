library(data.table)

context("NMreplaceDataFile")

test_that("basic",{

    fileRef <- "testReference/NMreplaceDataFile_01.rds"
    
    outfile <- "testOutput/NMreplaceDataFile_01.mod"

    NMreplaceDataFile(files="testData/nonmem/xgxr011.mod"
                     ,path.data="this/data.csv"
                     ,newfile=outfile
                      )
    res <- readLines(outfile)

    expect_equal_to_reference(res,fileRef,version=2)

    if(FALSE){
        res
        ref <- readRDS(fileRef)
        res[1:10]
        ref[1:10]
        length(res)
        length(ref)
    }

})


test_that("missing newfile",{

    fileRef <- "testReference/NMreplaceDataFile_02.rds"
    
    outfile <- "testOutput/NMreplaceDataFile_02.mod"
    file.orig <- "testData/nonmem/xgxr011.mod"
    file.copy(file.orig,outfile,overwrite=TRUE)

    ## weirdly this is returning a list. adding newfile seems to make it simplify.
    res <- NMreplaceDataFile(files=outfile
                            ,path.data="this/data.csv"
                             )

    ##  expect_equal(res[[1]],readRDS(fileRef)[[1]])
    expect_equal_to_reference(res,fileRef,version=2)

    if(FALSE){
        ## res
        ref <- readRDS(fileRef)
        res[[1]][1:10]
        ref[[1]][1:10]
        length(res[[1]])
        length(ref[[1]])
    }

    
}
)
