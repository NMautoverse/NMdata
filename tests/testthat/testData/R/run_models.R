library(devtools)

load_all("~/wdirs/NMsim",export_all = FALSE)

NMdataConf(path.nonmem = "/opt/NONMEM/nm75/run/nmfe75") ## path to NONMEM executable

NMexec("../nonmem/xgxr033.mod")
NMexec("../nonmem/xgxr035.mod",method.execute = "NMsim",path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
       sge=FALSE)

NMexec("../nonmem/xgxr053.mod",sge=F)
