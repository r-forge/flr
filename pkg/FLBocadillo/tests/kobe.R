library(akima)

load(paste(myDir,"/data/albBrp.RData",   sep=""))
load(paste(myDir,"/data/albProjCR.RData",sep=""))
load(paste(myDir,"/data/albProjBH.RData",sep=""))


t.<-kobe(lapply(lapply(albProjBH,window,start=2007),ssb),bmsy=refpts(albBrp)["msy","ssb",    1,drop=T],ylab="gg",p=F)
t.<-as.data.frame((lapply(lapply(albProjBH,window,start=2007),ssb)))
t.<-as.data.frame((lapply(lapply(albProjBH,window,start=2007),ssb)))
t2<-as.data.frame((lapply(lapply(albProjBH,window,start=2007),fbar)))
t3<-merge(t.,t2)
