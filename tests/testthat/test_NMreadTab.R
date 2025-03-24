## NMreadTab("testData/nonmem/estim_debug.cov")
context("NMreadTab")
NMdataConf(reset=TRUE)

### test we get TABLE.NO and NMREP right

test_that("Table with repetitions",{
    fileRef <- "testReference/NMreadTab_01.rds"
    
    res <- NMreadTab("testData/simulations/xgxr014_testsim1/xgxr014_testsim1.tab")

    expect_equal_to_reference(res,fileRef,version=2)
})


if(F){
    ## NOHEADER
    res <- NMreadTab("testData/nonmem/xgxr033_res_a.txt")
    ## ONEHEADER
    res <- NMreadTab("testData/nonmem/xgxr033_res_c.txt")
    ## NOLABEL
    res <-
        NMreadTab("testData/nonmem/xgxr033_res_d.txt")

    res <- NMscanTables("testData/nonmem/xgxr033.lst")
    lapply(res,head)

    res <- NMscanTables("testData/nonmem/xgxr033.lst",col.tableno=TRUE)
    lapply(res,head)
}


if(F){

### generating a plain csv without headers
### need NMsim 0.2.0
    ## library(NMsim)
    file.mod <- "testData/nonmem/xgxr032.mod"
    ## library(devtools)
    ## load_all("~/wdirs/NMsim")

    dt.sim <- NMcreateDoses(TIME=0,AMT=100) |>
        addEVID2(TIME=2,CMT=2,as.fun="data.table")
    dt.sim[,BBW:=80]

    res <- NMsim(file.mod=file.mod,
                 data=dt.sim,
                 dir.sims="testData/simulations",
                 name.sim="simpletab",
                 table.vars=c("PRED","IPRED"),
                 path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
                 seed.R=32,
                 sim.dir.from.scratch=T,
                 clean=5)
    
    res

    ## file.tab <- "testData/simulations/xgxr032_example_simpletab/xgxr032_example_simpletab.tab"
    ##file.copy(file.tab,"testData/data/")
}


test_that("table without table title",{
    fileRef <- "testReference/NMreadTab_02.rds"
    file.tab <- "testData/simulations/xgxr032_simpletab/xgxr032_simpletab.tab"
    ## file.tab <- "testData/data/xgxr032_simpletab.tab"
    ## readLines(file.tab)
    res <- NMreadTab(file.tab)

    expect_equal_to_reference(res,fileRef,version=2)

})
