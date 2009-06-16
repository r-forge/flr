################################################################################
#### Test                                                                   ####
################################################################################

## assumes that I´m getting a FLPar with the same dims as the object to be projected,
## although iters can be 1 or n

t.<-validSRPar(ple4,   FLPar(1:2,dimnames=list(params=c("a","b"),                          year=dimnames(m(ple4))$year)))
t.<-validSRPar(ple4,   FLPar(1:2,dimnames=list(params=c("a","b"),                          year=1:10)))
t.<-validSRPar(ple4,   FLPar(1:2,dimnames=list(params=c("a","b"),area=1:2,                 year=dimnames(m(ple4))$year)))
t.<-validSRPar(ple4sex,FLPar(1:2,dimnames=list(params=c("a","b"),unit=c("male","female"),  year=dimnames(m(ple4))$year)))
t.<-validSRPar(ple4sex,FLPar(1:2,dimnames=list(params=c("a","b"),unit=c("male","female"),  year=dimnames(m(ple4sex))$year)))
t.<-validSRPar(ple4,   FLPar(1:2,dimnames=list(params=c("a","b"),season=1:2,               year=dimnames(m(ple4))$year)))

flp<-FLPar(1:2,dimnames=list(params=c("a","b"),year=1990:1995,unit=1:2))
flp["a","1990",1:2]<-NA
flq<-as.FLQuant(flp)
flq["a","1990",1:2]<-NA

