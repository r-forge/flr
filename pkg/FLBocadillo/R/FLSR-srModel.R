# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT

# Reference:
# Notes: Updated to make generic

flagNormLog=FALSE

srr<-c("bevholt","ricker","cushing","shepherd","segreg","mean","dersh","pellat","bevholt","rickerD","cushingD","shepherdD")

setGeneric("sigma", function(obj, ...){
  standardGeneric("sigma")})
setMethod('sigma',
  signature(obj='FLQuant'),
    function(obj,hat=rep(0,length(obj))){
       ## calculates sigma squared for use in concentrated liklihood
       SS<-sum((obj-hat)^2,na.rm=T)

       return((SS/length(hat))^0.5)})

setGeneric("rSq", function(obs,hat){
  standardGeneric("rSq")})
setMethod('rSq',
  signature(obs='FLQuant',hat='FLQuant'),
    function(obs,hat=rep(0,length(obj))){
       ## calculates R squared
       mn   <-mean(obs)
       mnHat<-mean(hat)
       SStot<-sum((obs-mn)^2)
       SSreg<-sum((hat-mnHat)^2)
       SSerr<-sum((obs-hat)^2)
       
       res  <-1-SSerr/SStot
       
       return(res)})

setGeneric("loglAR1", function(obs,hat,rho,sigma){
  standardGeneric("loglAR1")})
setMethod('loglAR1',
  signature(obs='FLQuant',hat='FLQuant'),
    function(obs,hat,rho=0,sigma=NULL){
    ## calculates likelihood for AR(1) process

    if (is.null(sigma)) {
       sigma2<-sigma(obs,hat)
       sigma2<-sigma2^2}
    else
       sigma2<-sigma^2
       
    n        <-length(obs)
    s2       <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
    s1       <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
    sigma2.a <-(1-rho^2)*sigma2
    res      <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    return(res)})

sprFunc<-function(type,spr,a=NULL,b=NULL,c=NULL,d=NULL){
      # SSB as function of ssb/rec
      switch(type,
        "bevholt"  =a*(spr)-b,
        "ricker"   =log(a*spr)/b,
        "cushing"  =(1/(a*spr))^(1/(b-1)),
        "shepherd" =b*(a*spr-1)^(1/c),
        "segreg"   =ifelse(ssb <= b, a/(spr), 0),
        "mean"     =a/(spr),
        "dersh"    =ssb*a*(1-b*c*ssb)^c,
        "pellat"   =1/(a/ssb-a/ssb*(ssb/b)^c),
        "shepherdD"=NULL)}

srrFunc<-function(type,ssb=NULL,spr=NULL,a=NULL,b=NULL,c=NULL,d=NULL){
    #recruits as function of ssb or ssb/red
    if (is.null(ssb) & !is.null(spr))
       ssb<-sprFunc(type,spr,a,b,c)

      switch(type,
          "bevholt"  =a*ssb/(b+ssb),
          "ricker"   =a*ssb*exp(-b*ssb),
          "cushing"  =a*ssb^b,
          "shepherd" =a*ssb/(1+(ssb/b)^c),
          "segreg"   =ifelse(ssb<=b,a*ssb,a*b),
          "mean"     =a,
          "dersh"    =a*(1-b*c*ssb)^(1/c),
          "pellat"   =a*(1-(ssb/b)^c),
          "bevholtD" =a*ssb/(b+ssb),
          "rickerD"  =a*ssb*exp(-b*ssb),
          "shepherdD"=a*ssb^2/(1+(ssb/b)^c))}

sv<-function(type,spr0,a,b=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      if (type=="shepherd2"){
        v<-b*(a*spr0-1)^(1/c)
        s<-(0.2+0.2*(v/b)^c)/(1+(v*0.2/b)^c)
        res<-c(s,v)
        names(res)<-c("s","v")

        return(res)}

      v=sprFunc(type,spr0,a=a,b=b,c=c,d=d)
      s=srrFunc(type,ssb=v*.2,a=a,b=b,c=c,d=d)/srrFunc(type,ssb=v,a=a,b=b,c=c,d=d)

      res<-c(s,v)
      names(res)<-c("s","v")
      return(res)}

ab<-function(type,spr0,s=NULL,v=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      switch(type,
        "bevholt" ={a<-(v+(v-s*v)/(5*s-1))/spr0;             b<-(v-s*v)/(5*s-1)},
        "ricker"  ={b<-log(5*s)/(v*0.8);                     a<-exp(v*b)/spr0},
        "cushing" ={b<-log(s)/log(0.2);                      a<-(v^(1-b))/(spr0)},
        "shepherd"={b<-v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c));   a<-((v/b)^c+1)/spr0},
        "mean"    ={a<-v/spr0;                               b<-NULL},
        "segreg"  ={a<-5*s/spr0;                             b<-v/(a*spr0)},
        "pellat"  =stop("not done yet"),
        "dersch"  =stop("not done yet"))

        res<-cbind(a,b)
        return(res)}

modelFunc<-function(type,sv=FALSE){

    if (sv) return(paste(type,"SV",sep=""))
    
    ## SRR formulae
    switch(type,
        "bevholt"  =rec~a*ssb/(b+ssb),
        "ricker"   =rec~a*ssb*exp(-b*ssb),
        "cushing"  =rec~a*ssb^b,
        "shepherd" =rec~a*ssb/(1+(ssb/b)^c),
        "segreg"   =rec~ifelse(ssb <= b, a*ssb, a*b),
        "mean"     =rec~a,
        "dersh"    =rec~a*(1-b*c*ssb)^(1/c),
        "pellat"   =rec~a*(1-(ssb/b)^c),
        "bevholtD" =rec~a*ssb/(b+ssb),
        "rickerD"  =rec~a*ssb*exp(-b*ssb),
        "shepherdD"=rec~a*ssb^2/(1+(ssb/b)^c))}

loglFunc<-function(type){
    # Loglikelihoods
    switch(type,
        "normlog"   ="sum(dnorm(rec, hat, sigma, TRUE), na.rm=TRUE)",
        "normal"    ="sum(dnorm(rec, hat, sigma, FALSE), na.rm=TRUE)",
        "lognorm"   ="loglAR1(0,  sigma,rec,hat)",
        "normar1"   ="loglAR1(rho,sigma,rec,hat)",
        "lognormar1"="loglAR1(rho,sigma,rec),hat)")}

srModel<- function(mdl="bevholt",ll="normlog",sv=FALSE,ca=FALSE,cb=FALSE,cs=FALSE,cv=FALSE,ndc=FALSE){

    #### Creates model structures

    ## parameters
    args<-unlist(initialOps[mdl,ifelse(sv,"sv","ab")])

    if (ll %in% c("normar1","lognormar1")) args<-c(args,"rho")

    if (gregexpr("log", ll)[[1]][1]>0) {
       rec="log(rec)"
       hat="log(hat)"
    }else{
       rec="rec"
       hat="hat"}

    ## log likelihood, assuming normal log.
    cat("foo<-function(",args,",rec,ssb){\n",
                       ifelse(sv,"\t\tsv2ab(s,v,spr0)\n",""),
                       "\t\that  \t<-",ac(modelFunc(mdl))[[3]],"\n",
                       "\t\that  \t<-",hat,"\n",
                       "\t\trec  \t<-",rec,"\n",
                       "\t\tsigma\t<-sigma(rec, hat)\n",
                       "\t\tres  \t\t<-",loglFunc(ll),"\n",
                       "\t\treturn(res)}\n",sep="",file="foo.txt")

    source("foo.txt")
    logl<-foo

    cat(InitialModel(mdl,sv=sv,ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc),file="foo.txt")
    source("foo.txt")
    initial<-foo

	  return(list(logl=logl, model=modelFunc(mdl,sv), initial=initial))}

bevholt<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb){
       hat. <-log((a*ssb)/(b+ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)
       if (!is.finite(res)) res<--10e-200

    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

    return(list(a=a,b=b))},

  ## bounds
  lower=c(10e-8, 10e-8),
	upper=rep(Inf, 2))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)

	return(list(logl=logl, model=model, initial=initial))}

ricker<-function(){
 logl <- function(a, b, rec, ssb){
     hat. <-log(a*ssb*exp(-b*ssb))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(obs,hat.,0,sigma)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  initial <- structure(function(rec, ssb){
			x <- ssb
			y <- log(rec/ssb)

      res<-coefficients(lm(c(y)~c(x)))

      a<-max(exp(res[1]))
      b<--max(res[2])

			return(list(a=a,b=b))},

		# lower and upper limits for optim()
		lower=rep(1e-8,2),
		upper=c(Inf,Inf))

	model  <- rec~a*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}

segreg <- function(){
	logl <- function(a, b, rec, ssb){
     hat. <-FLQuant(log(ifelse(ssb <= b, a*ssb, a*b)))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(obs,hat.,0,sigma)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  model <- rec ~ FLQuant(ifelse(ssb <= b, a*ssb, a*b))

  initial <- structure(function(rec, ssb)
    {
    a <- median(c(rec/ssb),na.rm=T)
    b <- median(c(ssb)    ,na.rm=T)

    return(list(a=a,b=b))},

    lower=c(1e-8,0)  ,
    upper=c(Inf,Inf)
    #lower=c(exp(mean(log(rec(s.)),na.rm=T))/max(ssb(s.),na.rm=T),0)  ,
    #upper=c(exp(mean(log(rec(s.)),na.rm=T))/min(ssb(s.),na.rm=T),Inf)
    )

	return(list(logl=logl, model=model, initial=initial))}

shepherd<-function()
    {
    ## log likelihood, assuming normal log.
    logl <- function(a,b,c,rec,ssb){
       hat. <-log(a*ssb/(1+(ssb/b)^c))
       obs  <-log(rec)
  	   sigma<-sigma(obs,hat.)

  	   # minus log-likelihood
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial <- structure(
      function(rec,ssb){
        c<-2
        a<-quantile(c(rec/ssb^2),prob=0.75,na.rm=T)
        #b<-quantile(c(rec),prob=0.75,na.rm=T)/a

        b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)
        b<-b/quantile(c(ssb),prob=0.5,na.rm=T)

        names(a)<-NULL
        names(b)<-NULL

        return(list(a=a,b=b,c=c))
        },

      lower = c(1e-08, 1e-08, 1),
      upper = c(1e02,  1e+08,10))

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))}

.bevholt<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("bevholt",ssb=ssb,a=a,b=b)
   return(res)
   }

.ricker<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("ricker",ssb=ssb,a=a,b=b)
   return(res)
   }

.cushing<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("cushing",ssb=ssb,a=a,b=b)
   return(res)
   }

.shepherd<-function(ssb,a,b,c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("shepherd",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.segreg<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("segreg",ssb=ssb,a=a,b=b)
   return(res)
   }

.dersch<-function(ssb,a,b,c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("dersch",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.mean<-function(ssb,a,ca=0){
   a  <-a*(1+ca*covar)

   res<-srrFunc("bevholt",ssb=ssb,a=a,b=b)
   return(res)
   }

.pellat<-function(ssb,a,b,c=c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("pellat",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.bevholtSV<-function(ssb,spr0,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("bevholt",spr0,s=s,v=v)
   res<-srrFunc("bevholt",ssb=ssb,a=par["a"],b=par["b"])
   
   return(res)}

.rickerSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("ricker",spr0,s=s,v=v)
   res<-srrFunc("ricker",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.cushingSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("cushing",spr0,s=s,v=v)
   res<-srrFunc("cushing",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.shepherdSV<-function(ssb,s,v,c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("shepherd",spr0,s=s,v=v)
   res<-srrFunc("shepherd",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}

.segregSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("segreg",spr0,s=s,v=v)
   res<-srrFunc("segreg",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.derschSV<-function(ssb,s,v,c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("dersch",spr0,s=s,v=v)
   res<-srrFunc("dersch",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}

.meanSV<-function(ssb,v,cs=0){
   v  <-v*(1+cs*covar)

   par<-ab("mean",spr0,v=v)
   res<-srrFunc("bevholt",ssb=ssb,,a=par["a"])
   
   return(res)}

.pellatSV<-function(ssb,s,v,c=c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("pellat",spr0,s=s,v=v)
   res<-srrFunc("pellat",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}

#bevholt     <-function(mdl="bevholt"    ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
#ricker      <-function(mdl="ricker"     ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
#shepherd    <-function(mdl="shepherd"   ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
#segreg      <-function(mdl="segreg"     ,ll="normlog",sv=FALSE) stop("not yet implemented")
cushing     <-function(mdl="cushing"    ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
geomean     <-function(mdl="geomean"    ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
dersch      <-function(mdl="dersch"     ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
pellat      <-function(mdl="pellat"     ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)

bevholtSV   <-function(mdl="bevholt"    ,ll="normlog",sv=TRUE)  srModel(mdl,ll,sv)
rickerSV    <-function(mdl="ricker"     ,ll="normlog",sv=TRUE)  srModel(mdl,ll,sv)
shepherdSV  <-function(mdl="shepherdSV" ,ll="normlog",sv=TRUE)  srModel(mdl,ll,sv)
cushingSV   <-function(mdl="cushing"    ,ll="normlog",sv=TRUE)  srModel(mdl,ll,sv)

bevholtD    <-function(mdl="bevholtD"   ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
rickerD     <-function(mdl="rickerD"    ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)
shepherdD   <-function(mdl="shepherdD"  ,ll="normlog",sv=FALSE) srModel(mdl,ll,sv)

bevholtNDC  <-function(mdl="bevholtNDC" ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
rickerNDC   <-function(mdl="rickerNDC"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
shepherdNDC <-function(mdl="shepherdNDC",ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
cushingNDC  <-function(mdl="cushingNDC" ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
pellatNDC   <-function(mdl="pellatNDC"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
derschNDC   <-function(mdl="derschNDC"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
segregNDC   <-function(mdl="segregNDC"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)

bevholtCA   <-function(mdl="bevholtCA"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
rickerCA    <-function(mdl="rickerCA"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
shepherdCA  <-function(mdl="shepherdCA" ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
cushingCA   <-function(mdl="cushingCA"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
derschCA    <-function(mdl="derschCA"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
meanCA      <-function(mdl="meanCA"     ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
geomeanCA   <-function(mdl="geomeanCA"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
pellatCA    <-function(mdl="pellatCA"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
segregCA    <-function(mdl="segregCA"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)

bevholtCB   <-function(mdl="bevholtCB"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
rickerCB    <-function(mdl="rickerCB"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
shepherdCB  <-function(mdl="shepherdCB" ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
cushingCB   <-function(mdl="cushingCB"  ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
pellatCB    <-function(mdl="pellatCB"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)
segregCB    <-function(mdl="segregCB"   ,ll="normlog",sv=FALSE) stop("not yet implemented") #srModel(mdl,ll,sv)

#### Backwards compatibility
ricker.d    <-rickerD
ricker.c.a  <-rickerCA
ricker.c.b  <-rickerCB
ricker.sv   <-rickerSV
ricker.ar1  <-function(mdl="ricker"     ,ll="normlogar1",sv=FALSE) srModel(mdl,ll,sv)
bevholt.ar1 <-function(mdl="bevholt"    ,ll="normlogar1",sv=FALSE) srModel(mdl,ll,sv)
bevholt.d   <-bevholtD
bevholt.c.a <-bevholtCA
bevholt.c.b <-bevholtCB
bevholt.sv  <-bevholtSV
bevholt.ndc <-bevholtNDC
shepherd.ar1<-function(mdl="shepherd"   ,ll="normlogar1",sv=FALSE) srModel(mdl,ll,sv)
shepherd.d  <-shepherdD

