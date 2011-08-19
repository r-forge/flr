par(mfcol=c(3,2),mar=c(4,4,1,1))
r.<-seq(0.45,0.55,length.out=100)
K.<-seq(90,110, length.out=100)

r  <-mean(r.)
K  <-mean(K.)
args<-list(r=r,K=K)

######### Check derivatives ####################################################
#### r
x <-r
dx<-x*0.05
plot(r.,    fmsy("schaefer",r=r.,K=K),             xlab="r",ylab="Fmsy",type="l")
y <-fmsy("schaefer",r=r,K=K);gr<-do.call(msyDeriv[["schaefer"]][["fmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,     msy("schaefer",r=r.,K=K),             xlab="r",ylab="MSY", type="l")
y <-msy("schaefer",r=r,K=K,p=p);gr<-do.call(msyDeriv[["schaefer"]][["msy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,rep(bmsy("schaefer",r=r.,K=K),length(r.)), xlab="r",ylab="Bmsy",type="l")
y <-bmsy("schaefer",r=r,K=K,p=p);gr<-do.call(msyDeriv[["schaefer"]][["bmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### K
x <-K
dx<-x*0.05
plot(K., rep(fmsy("schaefer",r=r,K=K.),length(K.)),             xlab="K",ylab="Fmsy",type="l")
y <-fmsy("schaefer",r=r,K=K,p=p);gr<-do.call(msyDeriv[["schaefer"]][["fmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,     msy("schaefer",r=r,K=K.),             xlab="K",ylab="MSY", type="l")
y <-msy("schaefer",r=r,K=K,p=p);gr<-do.call(msyDeriv[["schaefer"]][["msy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,bmsy("schaefer",r=r,K=K.),                  xlab="K",ylab="Bmsy",type="l")
y <-bmsy("schaefer",r=r,K=K,p=p);gr<-do.call(msyDeriv[["schaefer"]][["bmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
