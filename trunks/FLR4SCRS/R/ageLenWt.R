################################################################################
#### Functions to generate ages from length etc ################################
################################################################################

### Age Slicing
setGeneric('ageIt', function(len,n,params,...)
   standardGeneric('ageIt'))
setMethod("ageIt", signature(len="numeric",n="numeric",params="FLPar"),
  function(len,n,params,timing=0.5,plusGroup=30){
     ## expected age at length adjust to beginning of year
     age=pmax(pmin(ceiling(params["t0"]-log(1-pmin(len/params["Linf"],.9999999))/params["K"]-timing),plusGroup),0)
#     age=params["t0"]-log(1-pmin(len/params["Linf"],.9999999))/params["K"]
#     age=pmax(pmin(round(age-timing),plusGroup),0)

     ## calculate frequencies
     frq=aggregate(n, list(age=age), sum)
     names(frq)[2]<-"freq"

     ## calculate mean length
     ln= aggregate(n*len, list(age=age), sum)[2]/
         aggregate(n,     list(age=age), sum)[2]

     return(data.frame(frq,len=ln))})
     
setMethod("ageIt", signature(len="data.frame",n="missing",params="FLPar"),
  function(len,params,timing=0.5,plusGroup=30)
     ageIt(len=len$len,n=len$n,params=params,timing=timing,plusGroup=plusGroup))

     
#### Statistical fitting using package mixdist
#### Wrapper function for plottin & returning a data.frame
setGeneric('ageThem', function(mixdat, ...)
   standardGeneric('ageThem'))
setMethod("ageThem", signature(mixdat="data.frame"),
  function(mixdat, mixpar, vB, ages, dist = "norm",
             constr =list(conpi="NONE",conmu="NONE",consigma="NONE",fixpi=NULL,fixmu=NULL,fixsigma=NULL,cov=NULL,size=NULL),
             emsteps=1, usecondit=FALSE, exptol=5e-06, print.level=0,title=NULL,line=-7,...){

      ## aggregate frequencies by bins
      mixdat=with(mixdat,aggregate(n,list(len=len),sum))
      names(mixdat)[2]<-"n"

      lnDist=try(mix(mixdat,mixpar,dist,constr,emsteps,usecondit,exptol,print.level))
#      lnDist=try(mix(mixdat,grwPar,"norm",constr=constr,emsteps=emsteps,usecondit=usecondit,exptol=exptol,print.level=print.level,...))

      if(!is(lnDist, 'try-error')){
        ## plot
        plot(lnDist,...)
        abline(v=vonB(ages-0.5,vB),col="green",lty=5)
        title(title,line=line,adj=1)

        return(data.frame(age=ages,lnDist$parameters,lnDist$se))} else
        return(data.frame(age=ages,mixpar,fail=TRUE))

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

#### plotting length Freqs
#setGeneric('diags', function(object,...)
#   standardGeneric('diags'))
#setMethod("diags", signature(object="mix"),
#  function(object,n,params,binwidth=1){
#    fn <-function(x,pop=1,mean=0.0,sd=1.0,pi=1.0) pi*dnorm(x,mean,sd)
#    fn2<-function(x,lf,binwidth) {
#           lp =lf$parameters
#           pop=sum(lf$mixdata[,2])*binwidth
#           res=mdply(lp,fn,x=x,pop=pop,mean=lp$mu,sd=lp$sigma,pi=pop*lp$pi)
#
#           res<-NULL
#           for (i in 1:dim(lp)[1])
#              res<-rbind(res,data.frame(mode=i,len=c(x,NA),freq=pop*c(fn(x,mean=lp$mu[i], sd=lp$sigma[i], pi=lp$pi[i]),NA)))
#
#           names(res)[2:3]<-names(lf$mixdata)
#           return(res)}
#
#    modes<-data.frame(fn2(sort(unique(object$mixdata[,1])),object,binwidth),  type="mode")
#    fit  <-data.frame(aggregate(modes[,3],by=list(len=modes[,2]),sum),type="fit", mode=0)
#    names(res)[1:2]<-names(object$mixdata)
#
#    res  <-rbind.fill(data.frame(object$mixdata,type="data",mode=0),mode=modes,fit=fit)
#
#    return(res[!is.na(res[,1]),])})

##### Inverse growth curve
#### Growth
setGeneric('invVonB', function(object, params, ...)
  standardGeneric('invVonB'))
setMethod("invVonB", signature(object="numeric", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLQuant", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLCohort", params="FLPar"),
   function(object,params) params["t0"]-log(1.0-object/params["Linf"])/params["K"])
setMethod("invVonB", signature(object="FLQuant",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})
setMethod("invVonB", signature(object="FLCohort",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})
setMethod("invVonB", signature(object="numeric",params="missing"),
   function(object,Linf=NA,K=NA,t0=NA) {
      params<-FLPar(Linf=Linf,K=K,t0=t0)
      params["t0"]-log(1.0-object/params["Linf"])/params["K"]})

setGeneric('invVonBMass', function(object, params, ...)
  standardGeneric('invVonBMass'))
setMethod("invVonBMass", signature(object="FLQuant", params="FLPar"),
   function(object,params) invVonB((object/params["a"])^(1.0/params["b"]),params))
setMethod("invVonBMass", signature(object="FLCohort", params="FLPar"),
   function(object,params) invVonB((object/params["a"])^(1.0/params["b"]),params))
setMethod("invVonBMass", signature(object="numeric", params="FLPar"),
   function(object,params) invVonB(c((object/params["a"])^(1.0/params["b"])),params))
setMethod("invVonBMass", signature(object="FLQuant", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return((object/params["a"])^(1.0/params["b"]))})
setMethod("invVonBMass", signature(object="FLCohort", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return((object/params["a"])^(1.0/params["b"]))})
setMethod("invVonBMass", signature(object="numeric", params="missing"),
   function(object,Linf=NA,K=NA,t0=NA,a=NA,b=NA){
      params<-FLPar(Linf=Linf,K=K,t0=t0,a=a,b=b)

      return(invVonB(c(object/params["a"])^(1.0/params["b"]),params))})

#      return(params["a"]*invVonB(object,params)^params["b"])})

