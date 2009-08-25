# estimated parameters
# look at likilihood profile
r.  <-seq(0.1, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(albBD,fix=c(r=x),start=c(r=.3, K=mean(catch(albBD))*10))@logLik})
plot(logl~r.,type="l")
points(c(albBD@params["r",]),albSP@logLik,pch=16,col="red",cex=2)
