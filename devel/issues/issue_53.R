library(devtools)

load_all("~/wdirs/NMdata")
unloadNamespace("NMdata")


## install.packages("NMdata")

install.packages("NMdata",repos="https://cran.r-project.org")
library(NMdata)

##library(NMdata)
chk <- readLines("https://raw.githubusercontent.com/dpastoor/ddmore_scraping/refs/heads/master/221/Executable_SLD_SUV_OS_GIST.mod")
inits <- NMdata::NMreadInits(lines=chk, section="omega")
head(inits)


##### check ddmore models



allmod <- list.files("~/wdirs/ddmore_scraping", pattern = "\\.mod$|\\.mdl", full.names = TRUE, recursive = TRUE)
allmod <- list.files("~/wdirs/ddmore_scraping", pattern = "\\.lst$", full.names = TRUE, recursive = TRUE)


library(data.table)

allmod <- list.files("~/wdirs/ddmore_scraping", pattern = "\\.mod$", full.names = TRUE, recursive = TRUE)
dt <- data.table(mod=allmod)[,n := .I]
dt[,ext := fnExtension(mod)]

dt[,(c("status","msg","nrow","ncol")) := {
    chk <- try(NMdata::NMreadInits(mod), silent=TRUE)
    if(inherits(chk,"try-error")){
        list("Error",chk,0,0)
    }else if(inherits(chk,"data.frame")){
        list("OK","",nrow(chk),ncol(chk))
    }
},by=n]

dt[,.N,keyby=.(ext,status)]
dt[ext=="mod",lapply(.SD,range),.SDcols=cc(nrow,ncol)]
## all meaningful numbers of parameters and columns
dt[ext=="mod",.N,keyby=.(nrow,ncol)]


load_all("~/wdirs/NMdata")

dt[ext=="mod"&status=="Error"]

dt[n==24,mod]
dt[n==37,mod]
res.24 <- dt[n==24,NMreadInits(mod)]
dt[n==24,NMreadParsText(mod,format="%comment")]
dt[n==37,NMreadInits(mod)]


dt[n==24,NMreadInits(lines=iconv(readLines(mod), "latin1", "ASCII", sub=""))]

mod1 <- dt[n==24,mod]
NMreadInits(file=mod1)
NMreadInits(lines=iconv(readLines(mod1), "latin1", "ASCII", sub=""))

NMreadParsText(mod1,format="%comment") |>
    mergeCheck(NMreadInits(mod1),by="parameter",common.cols="merge.by")

for(i in allmod){
    chk <- try(NMdata::NMreadInits(i), silent=TRUE)
    if(inherits(chk,"try-error")){
        cat("could not read",basename(i),"\n")
    }else if(inherits(chk,"data.frame")){
        cat("correctly read",basename(i),"\n")
    }
}


allmod[3]


NMreadInits(allmod[3])


##issue with non-ascii
~/wdirs/ddmore_scraping/228/Executable_run126h.mod


