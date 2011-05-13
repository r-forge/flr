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
### Steepness & Virgin Biomass #################################################
rkSV  <-srr
rkSVPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rkSV)){
    ssbRng          <-FLQuant(seq(0,max(ssb(rkSV[[i]]))*1.2,length.out=100))
    plot(rec(rkSV[[i]])~ssb(rkSV[[i]]),col="red",xlim=c(0,max(ssb(rkSV[[i]]))),ylim=c(0,max(rec(rkSV[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     rkSV[[i]]) <-rickerSV()
    try  <-try(rkSV[[i]]  <-fmle(rkSV[[i]]))
    #tryPS<-try(rkSVPS[[i]]<-fmle(rkSV[[i]],control=list(parscale=1/abs(computeD(rkSV[[i]])[1,1:2]))))
    #tryPS<-try(rkSVPS[[i]]<-fmle(rkSV[[i]],control=list(parscale=1/abs(diag(computeHessian(rkSV[[i]])))^0.5)))
    #tryPS<-try(rkSVPS[[i]]<-fmle(rkSV[[i]],control=list(parscale=1/abs(grad(func,x=c(rkSV[[i]]@initial(rkSV[[i]]@rec,rkSV[[i]]@ssb)))))))
    tryPS<-try(rkSVPS[[i]]<-fmle(rkSV[[i]],control=list(parscale=1/abs(grad(func,x=c(rkSV[[i]]@initial(rkSV[[i]]@rec,rkSV[[i]]@ssb)),sr=rkSV[[i]])))))

    if(!is(try,   'try-error')) lines(predict(rkSV[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(rkSVPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/rkSV.png",type="png")
save(rkSV,  file="C:/Stuff/FLR/tests/FLSR/Data/rkSV.RData")
save(rkSVPS,file="C:/Stuff/FLR/tests/FLSR/Data/rkSVPS.RData")

rkSVCF<-data.frame(rkSV  =unlist(lapply(rkSV,logLik)),
                 rkSVPS=unlist(lapply(rkSVPS,logLik)))

rkSVCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rkSV)){
    profile(rkSV[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/rkSVProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(rkSV)){
    profile(rkSVPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/rkSVProfilePS.png",type="png")