# FLash test script

library(FLCore)
library(FLash)
library(FLAssess)

data(ple4)
ple4.stf <- stf(ple4, nyears=3, wts.nyears=3)
# Stock now goes up up 2013
summary(ple4.stf)

proj.rec <- exp(mean(log(window(rec(ple4),start=2006,end=2008))))

# Catch target
ctrl<-fwdControl(data.frame(year=2009:2011,val=150000, quantity="catch"))
res<-fwd(ple4.stf,ctrl=ctrl,sr=list(model="mean",params=FLPar(proj.rec)))
plot(window(res,start=2000,end=2011))
catch(res)
fbar(res)




