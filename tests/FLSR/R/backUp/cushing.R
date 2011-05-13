library(FLCore)
library(FLBRP)


load("C:\\Stuff\\FLR\\tests\\FLSR\\Data\\srr.RData")

#### Function to calculate gradient for parameter scaling
func<-function(x,sr){
   rec=sr@rec
   ssb=sr@ssb
   res<-sr@logl(x[1],x[2],rec,ssb)
   return(res)}

### Cushing ####################################################################
ch  <-srr
chPS<-srr
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(ch)){
    ssbRng          <-FLQuant(seq(0,max(ssb(ch[[i]]))*1.2,length.out=100))
    plot(rec(ch[[i]])~ssb(ch[[i]]),col="red",xlim=c(0,max(ssb(ch[[i]]))),ylim=c(0,max(rec(ch[[i]]))),pch=19,xaxt ="n",yaxt="n")

    model(     ch[[i]]) <-cushing()
    try  <-try(ch[[i]]  <-fmle(ch[[i]]))
    #tryPS<-try(chPS[[i]]<-fmle(ch[[i]],control=list(parscale=1/abs(computeD(ch[[i]])[1,1:2]))))
    #tryPS<-try(chPS[[i]]<-fmle(ch[[i]],control=list(parscale=1/abs(diag(computeHessian(ch[[i]])))^0.5)))
    #tryPS<-try(chPS[[i]]<-fmle(ch[[i]],control=list(parscale=1/abs(grad(func,x=c(ch[[i]]@initial(ch[[i]]@rec,ch[[i]]@ssb)))))))
    tryPS<-try(chPS[[i]]<-fmle(ch[[i]],control=list(parscale=1/abs(grad(func,x=c(ch[[i]]@initial(ch[[i]]@rec,ch[[i]]@ssb)),sr=ch[[i]])))))

    if(!is(try,   'try-error')) lines(predict(ch[[  i]],ssb=ssbRng)~ssbRng,col="blue")
    if(!is(tryPS, 'try-error')) lines(predict(chPS[[i]],ssb=ssbRng)~ssbRng,col="white")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/ch.png",type="png")
save(ch,  file="C:/Stuff/FLR/tests/FLSR/Data/ch.RData")
save(chPS,file="C:/Stuff/FLR/tests/FLSR/Data/chPS.RData")

chCF<-data.frame(ch  =unlist(lapply(ch,logLik)),
                 chPS=unlist(lapply(chPS,logLik)))

chCF

#### Check likelihood profile ##################################################

## default
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(ch)){
    profile(ch[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot(C:/Stuff/FLR/tests/FLSR/Figs/chProfile.png",type="png")

## Parscaling
windows();par(mfrow=c(6,9),mar=c(0,0,0,0),bg="grey")
for (i in names(ch)){
    profile(chPS[[i]],which=c("a","b"),xaxt ="n",yaxt="n")
    mtext(i,side=3, line=-2)}
savePlot("C:/Stuff/FLR/tests/FLSR/Figs/chProfilePS.png",type="png")