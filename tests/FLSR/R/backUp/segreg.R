library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")


#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Segmented Regression #######################################################
sg  <-srr
sgPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sg)){
    ssbRng          <-FLQuant(seq(0,max(ssb(sg[[i]]))*1.2,length.out=100))
    plot(rec(sg[[i]])~ssb(sg[[i]]),col="red",xlim=c(0,max(ssb(sg[[i]]))),ylim=c(0,max(rec(sg[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     sg[[i]]) <-segreg()
    try  <-try(sg[[i]]  <-fmle(sg[[i]]))
    #tryPS<-try(sgPS[[i]]<-fmle(sg[[i]],control=list(parscale=1/abs(computeD(sg[[i]])[1,1:2]))))
    #tryPS<-try(sgPS[[i]]<-fmle(sg[[i]],control=list(parscale=1/abs(diag(computeHessian(sg[[i]])))^0.5)))
    #tryPS<-try(sgPS[[i]]<-fmle(sg[[i]],control=list(parscale=1/abs(grad(func,x=c(sg[[i]]@initial(sg[[i]]@rec,sg[[i]]@ssb)))))))
    tryPS<-try(sgPS[[i]]<-fmle(sg[[i]],control=list(parscale=1/abs(grad(func,x=c(sg[[i]]@initial(sg[[i]]@rec,sg[[i]]@ssb)),sr=sg[[i]])))))

    if(!is(try,   'try-error')) lines(predict(sg[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(sgPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/sg.png",type="png")
save(sg,  file="C:/Stuff/FLR/tests/FLSR/Data/sg.RData")
save(sgPS,file="C:/Stuff/FLR/tests/FLSR/Data/sgPS.RData")

sgCF<-data.frame(sg  =unlist(lapply(sg,logLik)),
                 sgPS=unlist(lapply(sgPS,logLik)))

sgCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sg)){
    profile(sg[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/sgProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(sg)){
    profile(sgPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/sgProfilePS.png",type="png")