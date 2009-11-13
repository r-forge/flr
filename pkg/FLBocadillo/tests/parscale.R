library(FLCore)

sr<- as.FLSR(ple4,model="bevholt")
sr<-transform(sr,rec=rec/1000,ssb=ssb/1000)
sr<- fmle(sr)
plot(sr)
sum(log((rec(sr)/fitted(sr)))^2)

ps <- auto_parscale(sr)
sr <- fmle(sr,control=list(parscale=ps,trace=10))
plot(sr)
sum(log((rec(sr)/fitted(sr)))^2)

sr <- as.FLSR(ple4,model="bevholt.sv")
sr <- fmle(sr,control=list(parscale = auto_parscale(sr)))
plot(sr)

sr <- as.FLSR(ple4,model="ricker")
sr <- fmle(sr,control=list(parscale = auto_parscale(sr)))
plot(sr)

sr <- as.FLSR(ple4,model="geomean")
sr <- fmle(sr,control=list(parscale = auto_parscale(sr)))
plot(sr)


library(akima)

par(mfrow=c(1,2))

sr<- as.FLSR(ple4,model="bevholt")
sr<-transform(sr,rec=rec/1000,ssb=ssb/1000)
sr<- fmle(sr)


# likelihood profile surface
pars<-expand.grid(a=params(sr)["a",]*seq(.8,1.2,length.out=10),
                  b=params(sr)["b",]*seq(.8,1.2,length.out=10))

ll<-function(x,pars,sr) fmle(sr,fixed=c(a=pars[x,"a"],b=pars[x,"b"]))@logLik

logl<-tapply(1:100,1:100,ll,pars=pars,sr=sr)

image(  interp(pars[,1], pars[,2],logl))
contour(interp(pars[,1], pars[,2],logl),add=T)
mtext("rec/1000, ssb/1000")
points(params(sr)["a",1],params(sr)["b",1],pch=19,cex=2)

sr <- as.FLSR(ple4,model="bevholt")
sr <- fmle(sr,control=list(parscale = auto_parscale(sr)))

pars<-expand.grid(a=params(sr)["a",]*seq(.8,1.2,length.out=10),
                  b=params(sr)["b",]*seq(.8,1.2,length.out=10))

logl<-tapply(1:100,1:100,ll,pars=pars,sr=sr)

image(  interp(pars[,1], pars[,2],logl))
contour(interp(pars[,1], pars[,2],logl),add=T)
points(params(sr)["a",1],params(sr)["b",1],pch=19,cex=2)
mtext("auto_parscale")


