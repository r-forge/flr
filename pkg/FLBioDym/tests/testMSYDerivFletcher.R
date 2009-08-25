par(mfcol=c(3,3),mar=c(4,4,1,1))
K.  <-seq(90,110, length.out=100)
msy.<-seq( 9, 11, length.out=100)
p.  <-seq(1.8,2.2,length.out=100)

K  <-mean(K.)
args<-list(K=K,msy=10,p=2)

######### Check derivatives ####################################################
#### r
x <-10
dx<-x*0.05
plot(msy.,  fmsy("fletcher",msy=msy.,K=K, p=p),             xlab="r",ylab="Fmsy",type="l")
y <-fmsy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["fmsy"]][["msy"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(msy.,     msy("fletcher",msy=msy.,K=K, p=p),             xlab="r",ylab="MSY", type="l")
y <-msy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["msy"]][["msy"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(msy.,rep(bmsy("fletcher",K=K, p=p),length(msy.)), xlab="r",ylab="Bmsy",type="l")
y <-bmsy("fletcher",K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["bmsy"]][["msy"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### p
x <-2
dx<-x*0.05
plot(p.,    fmsy("fletcher",msy=10,K=K, p=p.),             xlab="p",ylab="Fmsy",type="l")
y <-fmsy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["fmsy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(p.,rep(10,length(p.)),             xlab="p",ylab="MSY", type="l")
y <-msy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["msy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(p.,bmsy("fletcher",K=K, p=p.), xlab="p",ylab="Bmsy",type="l")
y <-bmsy("fletcher",K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["bmsy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### K
x <-K
dx<-x*0.05
plot(K.,    fmsy("fletcher",msy=10,K=K.,p=p),             xlab="K",ylab="Fmsy",type="l")
y <-fmsy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["fmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K., rep(10,length(K.)),             xlab="K",ylab="MSY", type="l")
y <-msy("fletcher",msy=10,K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["msy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,bmsy("fletcher",K=K.,p=p),                  xlab="K",ylab="Bmsy",type="l")
y <-bmsy("fletcher",K=K,p=p);gr<-do.call(msyDeriv[["fletcher"]][["bmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
