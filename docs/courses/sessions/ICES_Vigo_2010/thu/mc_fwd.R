#### Create a random variable for M
albM   <-alb20
m(albM)<-propagate(m(albM),100)

mDev<-rlnorm(prod(dim(m(albM))),0,0.3)
mean(mDev)
var(mDev)^.5

m(albM)<-m(albM)*FLQuant(mDev,dimnames=dimnames(m(albM)))
plot(m(albM))

ctrl<-fwdControl(data.frame(year=2009:2028,val=ctch,quantity="catch"))
albM     <-fwd(albM,ctrl=ctrl,sr=albSR)

harvest(albM)<-computeHarvest(albM)
catch(  albM)<-computeCatch(  albM,"all")

plot(FLStocks(albM))
plot(FLStocks(albM,iter(albM,12),iter(albM,20)))

ctrl<-fwdControl(data.frame(year=2009:2028,val=ctch,quantity="catch"))
albM     <-fwd(albM,ctrl=ctrl,sr=albSR)

plot(albM)