context("NMwriteSectionOne")

test_that("basic",{

    file.mod="testData/nonmem/xgxr011.mod"
    
    newlines <- "$INPUT ROW ID TIME EVID CMT"
    section <- "INPUT"


    lines <- readLines(file.mod,warn=FALSE)

    newmod <- NMwriteSectionOne(lines=lines
                               ,section=section
                               ,newlines=newlines
                                )
    
    res <- NMreadSection(lines=newmod,section=section)
    expect_equal(res,newlines)

})

test_that("section not matched",{

    file.mod="testData/nonmem/xgxr011.mod"
    
    newlines <- "$INPUT ROW ID TIME EVID CMT"
    section <- "INPUTTTTT"


    lines <- readLines(file.mod,warn=FALSE)

    newmod <- NMwriteSectionOne(lines=lines
                               ,section=section
                               ,newlines=newlines
                                )
    
    res <- NMreadSection(lines=newmod,section=section)
    
    expect_null(res)

})
