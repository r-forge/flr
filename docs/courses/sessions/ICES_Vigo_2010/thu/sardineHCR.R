t1  <-data.frame(year=2009:2014,val=1.0,quantity="f",  rel.year=2008:2013,min=NA,    max=NA)
t2  <-data.frame(year=2009:2014,val=NA, quantity="ssb",rel.year=NA,       min=200000,max=NA)
t3  <-data.frame(year=2009:2014,val=NA, quantity="f",  rel.year=NA,       min=0.1   ,max=0.5)
t4  <-rbind(t1,t2,t3)
t4  <-t4[order(t4[,"year"]),]
ctrl<-fwdControl(t4)

ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="mean",params=mnRec))
plot(ple4)
