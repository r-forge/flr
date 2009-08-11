################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

library(minpack.lm)
library(numDeriv)
library(FLCore)
library(FLAssess)
#library(FLPellaT)
library(akima)

myDir<-"C:\\Stuff\\FLR\\pkg\\FLBioDym"

source(paste(myDir,"R\\class.R",         sep="\\"))
source(paste(myDir,"R/constructors.R",   sep="\\"))
source(paste(myDir,"R/coerce.R",         sep="\\"))
source(paste(myDir,"R/createAccessors.R",sep="\\"))
source(paste(myDir,"R/methods.R",        sep="\\"))
source(paste(myDir,"R/sp.R",             sep="\\"))
source(paste(myDir,"R/fwd.R",            sep="\\"))
source(paste(myDir,"R/msy.R",            sep="\\"))
source(paste(myDir,"R/msyDeriv.R",       sep="\\"))
source(paste(myDir,"R/plot.R",           sep="\\"))
source(paste(myDir,"R/plotDiagnostics.R",sep="\\"))

#### Get data, and make catch & index globally availably in session #############
test  <-read.csv("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\test.csv")
catch <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=2001:2035))
stock <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=2001:2035))
idx   <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=2001:2035))
bdTest<-FLBioDym(stock=stock,catch=catch,index=idx)

save(bdTest,file=paste(myDir,"data/dbTest.RData",sep="\\"))
rm(test,stock,catch,idx)

load("C:\\Stuff\\FLR\\pkg\\FLBioDym\\data\\alb.RData")
load("C:\\Stuff\\FLR\\pkg\\FLBioDym\\data\\rocklob.RData")
load("C:\\Stuff\\FLR\\pkg\\FLBioDym\\data\\namhke.RData")

##### Fit Pella Tomlinson on test data
bdTest<-fit(bdTest,start=c(r=4.023034e-01,K=1.084226e+03))
plot(bdTest)
plot(bdTest,type="diag")
plot(bdTest,type="equil")

bd2<-fit(bdTest,fix=c(r=0.4,K=1000))
stock(bd2)

##### Double check by profiling liklihood
r.  <-seq(0.01, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(bdTest,fix=c(r=x),start=c(K=1e8))@LL})
plot(logl~r.,type="l")
points(c(bdTest@params["r",]),max(logl),pch=16,col="red",cex=2)

##### Surface
pars<-expand.grid(params(bdTest)["r",]*seq(0.75,1.25,length.out=10),
                  params(bdTest)["K",]*seq(0.75,1.25,length.out=10))
logl<-tapply(1:100,1:100, function(x) {fit(bdTest,fix=c(r=pars[x,"r"],K=pars[x,"K"]))}@LL)

image(  interp(pars[,1], pars[,2],logl))
contour(interp(pars[,1], pars[,2],logl),add=T)


##### Check with missing index years
bdTest.<-bdTest
index(bdTest.)<-index(bdTest.)[,3:30]
bdTest.<-fit(bdTest.)

##### Jacknife
bdTestJK<-bdTest
index(bdTestJK)<-jacknife(index(bdTestJK))
bdTestJK<-fit(bdTestJK,fix=c(r=0.4,K=1000))
x     <-bdTestJK


##### Fit FLSP on test data
sp       <-FLSP(catch=catch(bdTest),index=index(bdTest))
model(sp)<-pellatom()
sp@mpar  <- 2 # Schaeffer?

# try with scaled fmle
sp <- fmle_sc(sp,start=list(r=0.3,K=1000,Q=.2,sigma2=0.3),lower=c(r=.1,K=10,Q=0.01,sigma2=0.01),upper=c(r=1,K=1e6,Q=10,sigma2=10))
params(sp)
plot(sp)

#### Jacknife
t.<-t(msy(bdTest))

((sum(sweep(t.,2,apply(t.,2,mean),"-")^2))/(dim(t.)[1]*(dim(t.)[1]-1)))^.5

(apply(sweep(t.,2,apply(t.,2,mean),"-")^2,2,sum)/(dim(t.)[1]*(dim(t.)[1]-1)))^.5



    yrs    <-ac(dimnames(x@index)$year)

    obs    <-x@index[,yrs]
    resid  <-residuals(x)[,yrs]
    indVar <-x@stock[,yrs]
    indVar.<-FLQuant(seq(0, max(indVar), length=dim(indVar)[2]),dimnames=dimnames(indVar))
    hat    <-sweep(x@stock[,yrs],6, FLQuant(x@params["q",]), "*")
    prd    <-sweep(indVar.,6, FLQuant(x@params["q",]), "*")

    diagResidPlot(hat,indVar,indVar.,prd,obs,resid,xttl="Stock",yttl="CPUE",mttl="Index of abundance")
