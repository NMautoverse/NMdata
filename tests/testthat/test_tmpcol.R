context("tmpcol")

test_that("basic",{


    dt1 <- data.table(a=1,b=2)

    ## tmpcol2(data=dt1,base="a")
    ## tmpcol2(data=dt1,base=c("a","b"))
    ## tmpcol2(data=dt1,base=c("a","b","b"))
    ## tmpcol2(data=dt1,base=c("a","b","b","c"))
    ## tmpcol2(data=dt1,base=c("a","b","b","c"),prefer.plain=F)


    expect_equal(tmpcol(data=dt1,base="a"),"a1")

    expect_equal(tmpcol(data=dt1,base=c("a","b")),c("a1","b1"))
    expect_equal(tmpcol(data=dt1,base=c("a","b","b")),c("a1","b1","b2"))
    expect_equal(
        tmpcol(data=dt1,base=c("a","b","b","c")),c("a1","b1","b2","c"))
    expect_equal(
        tmpcol(data=dt1,base=c("a","b","b","c"),prefer.plain=F),c("a1","b1","b2","c1"))
    expect_equal(
        tmpcol(data=dt1,base=c("a","b","b","c"),sep=":"),c("a:1","b:1","b:2","c"))


})
