library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")


#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Mean #######################################################################
mn  <-srr
mnPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(mn)){
    ssbRng          <-FLQuant(seq(0,max(ssb(mn[[i]]))*1.2,length.out=100))
    plot(rec(mn[[i]])~ssb(mn[[i]]),col="red",xlim=c(0,max(ssb(mn[[i]]))),ylim=c(0,max(rec(mn[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     mn[[i]]) <-geomean()
    try  <-try(mn[[i]]  <-fmle(mn[[i]]))
    #tryPS<-try(mnPS[[i]]<-fmle(mn[[i]],control=list(parscale=1/abs(computeD(mn[[i]])[1,1:2]))))
    #tryPS<-try(mnPS[[i]]<-fmle(mn[[i]],control=list(parscale=1/abs(diag(computeHessian(mn[[i]])))^0.5)))
    #tryPS<-try(mnPS[[i]]<-fmle(mn[[i]],control=list(parscale=1/abs(grad(func,x=c(mn[[i]]@initial(mn[[i]]@rec,mn[[i]]@ssb)))))))
    tryPS<-try(mnPS[[i]]<-fmle(mn[[i]],control=list(parscale=1/abs(grad(func,x=c(mn[[i]]@initial(mn[[i]]@rec,mn[[i]]@ssb)),sr=mn[[i]])))))

    if(!is(try,   'try-error')) lines(predict(mn[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(mnPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/mn.png",type="png")
save(mn,  file="C:/Stuff/FLR/tests/FLSR/Data/mn.RData")
save(mnPS,file="C:/Stuff/FLR/tests/FLSR/Data/mnPS.RData")

mnCF<-data.frame(mn  =unlist(lapply(mn,logLik)),
                 mnPS=unlist(lapply(mnPS,logLik)))

mnCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(mn)){
    profile(mn[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/mnProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(mn)){
    profile(mnPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/mnProfilePS.png",type="png")