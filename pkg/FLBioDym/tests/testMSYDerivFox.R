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
plot(r.,    fmsy("fox",r=r.,K=K, p=p),             xlab="r",ylab="Fmsy",type="l")
y <-fmsy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["fmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,     msy("fox",r=r.,K=K, p=p),             xlab="r",ylab="MSY", type="l")
y <-msy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["msy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,rep(bmsy("fox",r=r.,K=K, p=p),length(r.)), xlab="r",ylab="Bmsy",type="l")
y <-bmsy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["bmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### K
x <-K
dx<-x*0.05
plot(K.,    fmsy("fox",r=r,K=K.,p=p),             xlab="K",ylab="Fmsy",type="l")
y <-fmsy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["fmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,     msy("fox",r=r,K=K.,p=p),             xlab="K",ylab="MSY", type="l")
y <-msy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["msy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,bmsy("fox",r=r,K=K.,p=p),                  xlab="K",ylab="Bmsy",type="l")
y <-bmsy("fox",r=r,K=K,p=p);gr<-do.call(msyDeriv[["fox"]][["bmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)