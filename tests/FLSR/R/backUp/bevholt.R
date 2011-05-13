library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")


#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Beverton & Holt ############################################################
bh  <-srr
bhPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(bh)){
    ssbRng          <-FLQuant(seq(0,max(ssb(bh[[i]]))*1.2,length.out=100))
    plot(rec(bh[[i]])~ssb(bh[[i]]),col="red",xlim=c(0,max(ssb(bh[[i]]))),ylim=c(0,max(rec(bh[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     bh[[i]]) <-bevholt()
    try  <-try(bh[[i]]  <-fmle(bh[[i]]))
    #tryPS<-try(bhPS[[i]]<-fmle(bh[[i]],control=list(parscale=1/abs(computeD(bh[[i]])[1,1:2]))))
    #tryPS<-try(bhPS[[i]]<-fmle(bh[[i]],control=list(parscale=1/abs(diag(computeHessian(bh[[i]])))^0.5)))
    #tryPS<-try(bhPS[[i]]<-fmle(bh[[i]],control=list(parscale=1/abs(grad(func,x=c(bh[[i]]@initial(bh[[i]]@rec,bh[[i]]@ssb)))))))
    tryPS<-try(bhPS[[i]]<-fmle(bh[[i]],control=list(parscale=1/abs(grad(func,x=c(bh[[i]]@initial(bh[[i]]@rec,bh[[i]]@ssb)),sr=bh[[i]])))))

    if(!is(try,   'try-error')) lines(predict(bh[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(bhPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/bh.png",type="png")
save(bh,  file="C:/Stuff/FLR/tests/FLSR/Data/bh.RData")
save(bhPS,file="C:/Stuff/FLR/tests/FLSR/Data/bhPS.RData")

bhCF<-data.frame(bh  =unlist(lapply(bh,logLik)),
                 bhPS=unlist(lapply(bhPS,logLik)))

bhCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(bh)){
    profile(bh[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/bhProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(bh)){
    profile(bhPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/bhProfilePS.png",type="png")