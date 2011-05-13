library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")

#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Ricker #####################################################################
rk  <-srr
rkPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rk)){
    ssbRng          <-FLQuant(seq(0,max(ssb(rk[[i]]))*1.2,length.out=100))
    plot(rec(rk[[i]])~ssb(rk[[i]]),col="red",xlim=c(0,max(ssb(rk[[i]]))),ylim=c(0,max(rec(rk[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     rk[[i]]) <-ricker()
    try  <-try(rk[[i]]  <-fmle(rk[[i]]))
    #tryPS<-try(rkPS[[i]]<-fmle(rk[[i]],control=list(parscale=1/abs(computeD(rk[[i]])[1,1:2]))))
    #tryPS<-try(rkPS[[i]]<-fmle(rk[[i]],control=list(parscale=1/abs(diag(computeHessian(rk[[i]])))^0.5)))
    #tryPS<-try(rkPS[[i]]<-fmle(rk[[i]],control=list(parscale=1/abs(grad(func,x=c(rk[[i]]@initial(rk[[i]]@rec,rk[[i]]@ssb)))))))
    tryPS<-try(rkPS[[i]]<-fmle(rk[[i]],control=list(parscale=1/pmax(0.000001,abs(grad(func,x=c(rk[[i]]@initial(rk[[i]]@rec,rk[[i]]@ssb)),sr=rk[[i]]))))))

    if(!is(try,   'try-error')) lines(predict(rk[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(rkPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/rk.png",type="png")
save(rk,  file="C:/Stuff/FLR/tests/FLSR/Data/rk.RData")
save(rkPS,file="C:/Stuff/FLR/tests/FLSR/Data/rkPS.RData")

rkCF<-data.frame(rk  =unlist(lapply(rk,logLik)),
                 rkPS=unlist(lapply(rkPS,logLik)))

rkCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rk)){
    profile(rk[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/rkProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rk)[50:51]){
    profile(rkPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/rkProfilePS.png",type="png")