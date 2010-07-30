################################################################################
#### Functions to generate ages from length etc ################################
################################################################################

### Age Slicing
setGeneric('ageIt', function(len,n, ...)
   standardGeneric('ageIt'))
setMethod("ageIt", signature(len="numeric",n="numeric"),
  function(len,n,Linf=NULL,K=NULL,t0=0.0,timing=0.5,plusGroup=30){
     ## expected age at length adjust to beginning of year
     age=pmax(pmin(floor(t0-log(1-pmin(len/Linf,.9999999))/K+timing),plusGroup),0)

     ## calculate frequencies
     res=aggregate(n, list(age=age), sum)

     return(res)})
setMethod("ageIt", signature(len="data.frame",n="missing"),
  function(len,Linf=NULL,K=NULL,t0=0.0,timing=0.5,plusGroup=30)
     ageIt(len[,1],len[,2],params=NULL,Linf=NULL,K=NULL,t0=0.0,timing=0.5,plusGroup=30))

     
#### Statistical fitting using package mixdist
#### Wrapper function for plottin & returning a data.frame
setGeneric('ageThem', function(mixdat, ...)
   standardGeneric('ageThem'))
setMethod("ageThem", signature(mixdat="data.frame"),
  function(mixdat, mixpar, vB, ages, dist = "norm", constr = list(conpi = "NONE",
    conmu = "NONE", consigma = "NONE", fixpi = NULL, fixmu = NULL,
    fixsigma = NULL, cov = NULL, size = NULL), emsteps = 1, usecondit = FALSE,
    exptol = 5e-06, print.level = 0, ...){


      ## aggregate frequencies by bins
      tst   =with(x,aggregate(n,list(len=len),sum))
      lnDist=mix(tst,grwPar,"norm",constr=constr,emsteps=3,print.level=1)

      plot(lnDist,xlim=c(50,250))
      abline(v=vonBert(ages,vB["Linf"],vB["K"],vB["t0"]),col="green",lty=5)
      mtext(unique(x$year))

      res<-data.frame(age=ages,lnDist$parameters,lnDist$se)
      return(res)})

#### using ADMB
#dyn.load("C:/Stuff/FLR/pkg/ADMB/inst/admb/ageThem/ageThemDLL.dll")

#ageThemAD<-function(bins,n,plusGroup,Linf,K,t0,sigmaLinf,timing=0.0){
#  cat("ageing them")
#  n[is.na(n)]<-0
#  hat<-array(0.1,c(1,plusGroup+1))
#  system.time(res<-.C("ageThemDLL",nages=as.integer(plusGroup),nsets=as.integer(1),nbins=as.integer(length(bins)),bins=as.double(bins),n=as.double(n),hat=as.double(hat),linf=as.double(Linf),K=as.double(K),t0=as.double(t0),timing=as.double(timing),as.double(sigmaLinf)," -sp -crit 1.e-8 -nohess"))
#  cat("\n")

#  return(res$hat)}
################################################################################

#### plotting length Freqs
plotLF<-function(lf,binwidth=1){
    fn <-function(x,pop=1,mean=0.0,sd=1.0,pi=1.0) pi*dnorm(x,mean,sd)
    fn2<-function(x,lf,binwidth) {
           lp =lf$parameters
           pop=sum(lf$mixdata[,2])*binwidth
#           res=mdply(lp,fn,x=x,pop=pop,mean=lp$mu,sd=lp$sigma,pi=pop*lp$pi)

           res<-NULL
           for (i in 1:dim(lp)[1])
              res<-rbind(res,data.frame(i=i,len=c(x,NA),freq=pop*c(fn(x,mean=lp$mu[i], sd=lp$sigma[i], pi=lp$pi[i]),NA)))

           return(res)}

    modes<-fn2(sort(unique(lf$mixdata[,1])),lf,binwidth)
    fit  <-aggregate(modes$freq,by=list(len=modes$len),sum)

    print(ggplot(lf$mixdata) +
           geom_histogram(aes(len,weight=n),colour = "darkgreen", fill = "grey", binwidth=binwidth) +
           geom_line(data=modes, aes(len,freq,group=i), colour="red",   size=1.25)+
           geom_line(data=fit,   aes(len,x),            colour="green", size=2))}


# FLPar(vector)
setMethod('FLPar', signature('vector'),
	function(object, params= if(length(names(object))==length(object)) names(object) else letters[seq(length(object)/length(iter))], iter=1,
    dimnames=list(params=params, iter=seq(iter)), byrow=FALSE, units='NA'){
    # if length(iter) == 1, then expand
    if(length(iter) == 1 && as.character(iter) != '1')
      iter <- seq(iter)

		res <- array(object,dim=unlist(lapply(dimnames, length)))
		return(FLPar(res, units=units, dimnames=dimnames))})
