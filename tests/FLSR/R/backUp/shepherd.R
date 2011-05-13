library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")


#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Shepherd ###################################################################
sh  <-srr
shPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sh)){
    ssbRng          <-FLQuant(seq(0,max(ssb(sh[[i]]))*1.2,length.out=100))
    plot(rec(sh[[i]])~ssb(sh[[i]]),col="red",xlim=c(0,max(ssb(sh[[i]]))),ylim=c(0,max(rec(sh[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     sh[[i]]) <-segreg()
    try  <-try(sh[[i]]  <-fmle(sh[[i]]))
    #tryPS<-try(shPS[[i]]<-fmle(sh[[i]],control=list(parscale=1/abs(computeD(sh[[i]])[1,1:2]))))
    #tryPS<-try(shPS[[i]]<-fmle(sh[[i]],control=list(parscale=1/abs(diag(computeHessian(sh[[i]])))^0.5)))
    #tryPS<-try(shPS[[i]]<-fmle(sh[[i]],control=list(parscale=1/abs(grad(func,x=c(sh[[i]]@initial(sh[[i]]@rec,sh[[i]]@ssb)))))))
    tryPS<-try(shPS[[i]]<-fmle(sh[[i]],control=list(parscale=1/abs(grad(func,x=c(sh[[i]]@initial(sh[[i]]@rec,sh[[i]]@ssb)),sr=sh[[i]])))))

    if(!is(try,   'try-error')) lines(predict(sh[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(shPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/sh.png",type="png")
save(sh,  file="C:/Stuff/FLR/tests/FLSR/Data/sh.RData")
save(shPS,file="C:/Stuff/FLR/tests/FLSR/Data/shPS.RData")

shCF<-data.frame(sh  =unlist(lapply(sh,logLik)),
                 shPS=unlist(lapply(shPS,logLik)))

shCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sh)){
    profile(sh[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/shProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sh)){
    profile(shPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/shProfilePS.png",type="png")