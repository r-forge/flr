# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, Cefas
# $Id$

# Reference:
# Notes:

# spr0:  calcs spawner per recruit at F=0.0
if (!isGeneric("spr0"))
  setGeneric("spr0", function(ssb, rec, fbar, ...)
	  standardGeneric("spr0"))

setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
   function(ssb, rec, fbar,log=TRUE){
    if  (any(dim(ssb)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(rec)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(fbar)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"

    # years: corrects length if mismatch
    minyear <- max(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) min(as.numeric(dimnames(x)$year)))))
    maxyear <- min(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) max(as.numeric(dimnames(x)$year)))))

    # ssb & f
    ssb  <- ssb[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    rec  <- rec[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    fbar <- fbar[1, as.character(seq(minyear, maxyear)), drop=TRUE]

    # spr0
    dmns     <-dimnames(ssb(x))
    dmns$year<-1
    res      <-FLQuant(NA,dimnames=dmns)

    if (log) {
       calcSpr0 <-function(x) exp(lm(log(c(ssb(x)/rec(x)))~c(fbar(x)))$coefficients[1])
       for (i in dmns$iter)
         res[,,,,,i]<-calcSpr0(iter(x,i))}
    else {
       calcSpr0 <-function(x) lm(c(ssb(x)/rec(x))~c(fbar(x)))$coefficients[1]
       for (i in dmns$year)
         res[,,,,,i]<-calcSpr0(iter(x,i))}

    return(res)})

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb,log=TRUE){
    sr <- as.FLSR(ssb)

    spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb),log)})

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar){
    spr0(ssb=ssb(ssb), rec=rec(ssb), fbar=fbar)})

# calc steepness & virgin biomass from alpha & beta, given SSB per R at F=0
sv<-function(type,spr0,a,b=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
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
        "dersch"  =stop("not done yet"),
        "bevholtD"  =stop("not done yet"),
        "rickerD"  =stop("not done yet"),
        "shepherD"  =stop("not done yet"))

        res<-cbind(a,b)
        return(res)}

########################################################################################################################
## log likelihood, assuming normal log.
flagNormLog<-TRUE

bevholt<-function(){
  logl <- function(a, b, rec, ssb){
       hat. <-log((a*ssb)/(b+ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

       if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
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
     if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
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
		lower=rep(1e-6,2),
		upper=c(Inf,Inf))

	model  <- rec~a*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}

segreg <- function(){
	logl <- function(a, b, rec, ssb){
     hat. <-FLQuant(log(ifelse(ssb <= b, a*ssb, a*b)))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
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
    )

	return(list(logl=logl, model=model, initial=initial))}

shepherd<-function()
    {
    logl <- function(a,b,c,rec,ssb){
       hat. <-log(a*ssb/(1+(ssb/b)^c))
       obs  <-log(rec)
  	   sigma<-sigma(obs,hat.)

  	   # minus log-likelihood
       if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial <- structure(
      function(rec,ssb){
        c<-1
        a<- quantile(c(rec/ssb),prob=0.75,na.rm=T)
        b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)

        names(a)<-NULL
        names(b)<-NULL

        return(list(a=a,b=b,c=c))
        },

      lower = c(1e-08, 1e-08, 1),
      upper = c(1e02,  1e+08,10))

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))}
