par(mfcol=c(3,3),mar=c(4,4,1,1))
r.<-seq(0.45,0.55,length.out=100)
K.<-seq(90,110, length.out=100)
p.<-seq(1.8,2.2,length.out=100)

r  <-mean(r.)
K  <-mean(K.)
p  <-mean(p.)
args<-list(r=r,K=K,p=p)

######### Check derivatives ####################################################
#### r
x <-r
dx<-x*0.05
plot(r.,    fmsy("pellat",r=r.,K=K, p=p),             xlab="r",ylab="Fmsy",type="l")
y <-fmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["fmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,     msy("pellat",r=r.,K=K, p=p),             xlab="r",ylab="MSY", type="l")
y <-msy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["msy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(r.,rep(bmsy("pellat",r=r.,K=K, p=p),length(r.)), xlab="r",ylab="Bmsy",type="l")
y <-bmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["bmsy"]][["r"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### p
x <-p
dx<-x*0.05
plot(p.,    fmsy("pellat",r=r,K=K, p=p.),             xlab="p",ylab="Fmsy",type="l")
y <-fmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["fmsy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(p.,     msy("pellat",r=r,K=K, p=p.),             xlab="p",ylab="MSY", type="l")
y <-msy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["msy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(p.,bmsy("pellat",r=r,K=K, p=p.), xlab="p",ylab="Bmsy",type="l")
y <-bmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["bmsy"]][["p"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### K
x <-K
dx<-x*0.05
plot(K.,    fmsy("pellat",r=r,K=K.,p=p),             xlab="K",ylab="Fmsy",type="l")
y <-fmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["fmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,     msy("pellat",r=r,K=K.,p=p),             xlab="K",ylab="MSY", type="l")
y <-msy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["msy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
plot(K.,bmsy("pellat",r=r,K=K.,p=p),                  xlab="K",ylab="Bmsy",type="l")
y <-bmsy("pellat",r=r,K=K,p=p);gr<-do.call(msyDeriv[["pellat"]][["bmsy"]][["K"]],args)
lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)

#### Numeric as double check
#plot(p.,fmsy("pellat",r=.5, K=100,p=p.),xlab="p",ylab="Fmsy",type="l")
#fn <-function(r,K,x) fmsy("pellat",r=r,K=K,p=x)
#x  <-mean(p.)
#y  <-fmsy("pellat",r=.5,K=100,p=x)
#gr <-genD(fn,x=2,r=.5,K=100)$D[1,1]
#lines(c(x-dx,x,x+dx),c(y-dx*gr,y,y+dx*gr),col="red",lwd=2)
