library(devtools)

load_all("~/wdirs/NMsim",export_all = FALSE)
library(NMsim)

path.nonmem <- NMsim:::prioritizePaths(c(
                           "/opt/NONMEM/nm75/run/nmfe75"
                          ,"/opt/nonmem/nm751/run/nmfe75"
                       ))

NMdataConf(path.nonmem = path.nonmem) ## path to NONMEM executable

NMexec("../nonmem/xgxr033.mod")
NMexec("../nonmem/xgxr035.mod",method.execute = "NMsim",path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
       sge=FALSE)

## occ var
NMexec("../nonmem/xgxr044.mod",sge=F)

NMexec("../nonmem/xgxr053.mod",sge=F)

## PREDPP model
NMexec("../nonmem/pred030.mod",sge=FALSE)

## IGNORE on column with NA
NMexec("../nonmem/xgxr054.mod",sge=FALSE)

NMexec("../nonmem/xgxr055.mod",sge=FALSE)
### dropping, then overwriting nomtime. using a new name in filters
NMexec("../nonmem/xgxr056.mod",sge=FALSE)

NMexec("../nonmem/xgxr059.mod",sge=FALSE)
