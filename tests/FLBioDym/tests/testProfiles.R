library(akima)

# likelihood profile surface
x<-tst1
pars<-expand.grid(r=params(x)["r",]*seq(.9,1.1,length.out=10),
                  K=params(x)["K",]*seq(.9,1.1,length.out=10))

ll<-function(x,pars,y){fit(y,fixed=c(r=pars[x,"r"],K=pars[x,"K"]))@logLik}
logl<-tapply(1:100,1:100,ll,pars=pars,y=tanBD)

image(  interp(pars[,1], pars[,2],logl))
contour(interp(pars[,1], pars[,2],logl),add=T)

# single parameter profile
r.  <-seq(0.1, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(albBD,fixed=c(r=x),start=c(r=.3, K=mean(catch(albBD))*10))@logLik})
plot(logl~r.,type="l")
points(c(albBD@params["r",]),albSP@logLik,pch=16,col="red",cex=2)

# CI
ll<-logLik(tst1)
fn<- function(x, bd) {(fit(bd,fixed=c(msy=x))@logLik - (ll-3.84/2))^2}
optimise(f=fn, interval=c(c(params(tst1)["msy",])*.5,  c(params(tst1)["msy",])), bd=tst1)
optimise(f=fn, interval=c(c(params(tst1)["msy",]),   2*c(params(tst1)["msy",])), bd=tst1)




