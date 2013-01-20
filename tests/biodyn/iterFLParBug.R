control=FLPar(array(rep(c(1,NA,NA,NA),each=4), dim=c(4,4,1), dimnames=list(params=c("r","k","p","b0"),option=c("phase","min","val","max"),iter=1)))
control=propagate(control,2)
control["r","val"]=c(1,2)
control

control["r",,1]=control["r",,1]
control

control["r","val"]=c(1,2)
iter(control,1)=iter(control,1)
control

as(as.data.frame(FLPar(a=1,b=2)),"FLPar")



