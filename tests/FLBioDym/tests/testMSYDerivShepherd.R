par(mfcol=c(3,3),mar=c(4,4,1,1))
r.<-seq(0.45,0.55,length.out=100)
K.<-seq(90,110, length.out=100)
m.<-seq(.125,.375,length.out=100)

r  <-mean(r.)
K  <-mean(K.)
m  <-mean(m.)
args<-list(r=r,K=K,m=m)

######### Check derivatives ####################################################
#### r
x <-r
dx<-x*0.05
plot(r.,    fmsy("shepherd",r=r.,K=K, m=m),             xlab="r",ylab="Fmsy",type="l")
y <-fmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["fmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,     msy("shepherd",r=r.,K=K, m=m),             xlab="r",ylab="MSY", type="l")
y <-msy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["msy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,bmsy("shepherd",r=r.,K=K, m=m), xlab="r",ylab="Bmsy",type="l")
y <-bmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["bmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### m
x <-m
dx<-x*0.05
plot(m.,    fmsy("shepherd",r=r,K=K, m=m.),             xlab="m",ylab="Fmsy",type="l")
y <-fmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["fmsy"]][["m"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(m.,     msy("shepherd",r=r,K=K, m=m.),             xlab="m",ylab="MSY", type="l")
y <-msy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["msy"]][["m"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(m.,bmsy("shepherd",r=r,K=K, m=m.), xlab="m",ylab="Bmsy",type="l")
y <-bmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["bmsy"]][["m"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### K
x <-K
dx<-x*0.05
plot(K.,    fmsy("shepherd",r=r,K=K.,m=m),             xlab="K",ylab="Fmsy",type="l")
y <-fmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["fmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,     msy("shepherd",r=r,K=K.,m=m),             xlab="K",ylab="MSY", type="l")
y <-msy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["msy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,bmsy("shepherd",r=r,K=K.,m=m),                  xlab="K",ylab="Bmsy",type="l")
y <-bmsy("shepherd",r=r,K=K,m=m);gr<-do.call(msyDeriv[["shepherd"]][["bmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
