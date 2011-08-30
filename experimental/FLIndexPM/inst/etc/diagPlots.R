psr<-as.FLSR(ple4)
model(psr)<-ricker()
psr<-transform(psr,ssb=ssb/1000,rec=rec/1000)
psr<-fmle(psr)
plot(psr)

x<-psr

yrs    <-range(x,"minyear"):(range(x,"maxyear")-1)
hat    <-fitted(x)[,  ac(yrs)]
indVar <-ssb(x)[,     ac(yrs)]
indVar.<- FLQuant(seq(0, max(indVar), length=dim(indVar)[2]),dimnames=dimnames(indVar))
prd    <-predict(x, ssb=indVar.)
obs    <-x@rec[,      ac(yrs)]
resid  <-x@residuals[,ac(yrs)]

diagResidPlot(yrs,hat,indVar,indVar.,prd,obs,resid,xttl="SSB",yttl="Recruits",mttl="Stock Recruitment")

x<-flpt

 		yrs    <-range(x,"minyear"):(range(x,"maxyear"))
    indVar <-stock(x)
    indVar.<-FLQuant(seq(0, max(indVar), length=dim(indVar)[2]),dimnames=dimnames(indVar))
    obs    <-x@index[,    ac(yrs)]
    resid  <-x@residuals[,ac(yrs)]

    mnBio.<-mnBio(c(stock(x)))
    catchability<-calcQ(mnBio.,c(obs[,-dim(obs)[2]]),error="LOG")

    hat    <-FLQuant(catchability*mnBio.,dimnames=dimnames(obs))
    prd    <-indVar.*c(param(x)["q",])

diagResidPlot(yrs,hat,indVar,indVar.,prd,obs,resid,xttl="Stock",yttl="CPUE",mttl="Index of abundance")

