################################################################################
## MSE for swordfish ###########################################################
################################################################################

## Number of iterations
nIters     <-10

## Year range
nYrs       <-20
projYrs    <-2007:2028

## Set up operating model
OM         <-stf(swo,nYrs)        ## extend by 20 years
OM         <-propagate(OM,nIters) ## add iterations

## Stock recruitment
srr        <-as.FLSR(swo)
model(srr) <-bevholt()
srr        <-fmle(srr)
srPar      <-params(srr)
OMrefpts   <-brp(FLBRP(OM,srr))

srRsdl     <-FLQuant(sample(c(exp(residuals(srr))),length(projYrs)*nIters,TRUE),
                     dimnames=list(year=projYrs,iter=1:nIters))

## Run Projections by F
Projswo<-FLStocks()
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.4))
Projswo[["0.4"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.5))
Projswo[["0.5"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.6))
Projswo[["0.6"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.7))
Projswo[["0.7"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.8))
Projswo[["0.8"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*0.9))
Projswo[["0.9"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)
ctrl<-fwdControl(data.frame(year=as.numeric(ac(projYrs)),quantity="f",val=c(refpts(OMrefpts)["msy","harvest",1,drop=T])*1.0))
Projswo[["1.0"]]<-fwd(OM, ctrl=ctrl,sr=srr, sr.residuals=srRsdl)

plot(Projswo)

## summarise data
resultsProj<-ldply(Projswo,function(x) model.frame(FLQuants(SSB =sweep(ssb( x),6,refpts(OMrefpts)["msy","ssb"],    "/"),
                                                            FBar=sweep(fbar(x),6,refpts(OMrefpts)["msy","harvest"],"/"))))


## plot Kobe Phase plot
p<-kobe(resultsProj)
class(p)
p+geom_point(aes(SSB,FBar,group=.id,col=.id))+
  geom_line(aes(SSB,FBar),col="black",data=results[results$year<=2007,])+
  facet_wrap(~.id)

## Kobe matrix
## Probability of over fishing
resultsProj$pF<- 1-pmin(floor(resultsProj$FBar),1)

## Probability of over being over fished
resultsProj$pS<-   pmin(floor(resultsProj$SSB),1)

## Probability of being in the green zone
resultsProj$pRecovery<-resultsProj$pS*resultsProj$pF
head(results)

## calc P() by year & F
x  <-with(resultsProj[resultsProj$year>=2008,],aggregate(pRecovery,list(Year=year,F=.id),mean))
x$F<-as.numeric(as.character(x$F))

## Kobe Strategey Matrix
kobeM(x)

## Run MSEs ####################################################################
MSEswo<-FLStocks()
MSEswo[["0.4"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.4)$OM
MSEswo[["0.5"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.5)$OM
MSEswo[["0.6"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.6)$OM
MSEswo[["0.7"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.7)$OM
MSEswo[["0.9"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.9)$OM
MSEswo[["0.9"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=0.8)$OM
MSEswo[["1.0"]]<-runMSE(OM, projYrs[1], srPar, srRsdl=srRsdl,fmult=1.0)$OM

plot(MSEswo)

## plot Kobe Phase plot
p<-kobe(resultsMSE)
class(p)
p+geom_point(aes(SSB,FBar,group=.id,col=.id))+
  geom_line(aes(SSB,FBar),col="black",data=results[results$year<=2007,])+
  facet_wrap(~.id)

## Kobe matrix
## Probability of over fishing
resultsMSE$pF<- 1-pmin(floor(resultsMSE$FBar),1)

## Probability of over being over fished
resultsMSE$pS<-   pmin(floor(resultsMSE$SSB),1)

## Probability of being in the green zone
resultsMSE$pRecovery<-resultsMSE$pS*resultsMSE$pF
head(results)

## calc P() by year & F
x  <-with(resultsMSE[resultsMSE$year>=2008,],aggregate(pRecovery,list(Year=year,F=.id),mean))
x$F<-as.numeric(as.character(x$F))

## Kobe Strategey Matrix
kobeM(x)



