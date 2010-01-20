# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT

# Reference:
# Notes: Updated to make generic

#### Utility functions
## Gets formula from names
SRModelName<-function(formula){
  srmodels <- list('bevholt'  ,'bevholt.ar1' ,'bevholt.d', 'bevholt.sv' ,'bevholt.c.a','bevholt.c.b', 'bevholt.ndc',
                   'ricker'   ,'ricker.ar1'  ,'ricker.d',  'ricker.sv'  ,'ricker.c.a', 'ricker.c.b',
                   'shepherd' ,'shepherd.ar1','shepherd.d','shepherd.sv',
                   'cushing'  ,'cushing.ar1' ,             'cushing.sv' ,
                   'mean'     ,'mean.ar1'    ,
                   'geomean'  ,'geomena.ar1' ,
                   'segreg'   ,'segreg.ar1'  ,             'segreg.sv')

  srformulae        <-lapply(srmodels, function(x) do.call(x, list())$model)
  names(srformulae) <-srmodels

  for(i in srmodels)
    if(formula == srformulae[[i]])
      return(i)

  return(FALSE)}

srr<-c("bevholt","ricker","cushing","shepherd","segreg","mean","dersh","pellat","bevholt","rickerD","cushingD","shepherdD")

setGeneric("sigma", function(obj, ...){
 standardGeneric("sigma")})

setMethod('sigma',
  signature(obj='FLQuant'),
    function(obj,hat=rep(0,length(obj))){
   ## calculates sigma squared for use in concentrated liklihood

   SS   <-sum((obj-hat)^2,na.rm=T)

   return((SS/length(hat))^0.5)})

logl.ar1<-function(rho,sigma,obs,hat){
    ## likelihood for AR(1) process
    sigma2   <-sigma^2
    n        <-length(obs)
    s2       <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
    s1       <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
    sigma2.a <-(1-rho^2)*sigma2
    res      <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    return(res)}

sprFunc<-function(type,spr,a=NULL,b=NULL,c=NULL,d=NULL){
      # SSB as function of ssb/rec
      switch(type,
        "bevholt"  =a*(spr)-b,
        "ricker"   =log(a*spr)/b,
        "cushing"  =(1/a)*spr^(b-1),
        "shepherd" =b(a*(spr)-1)^(1/c),
        "segreg"   =ifelse(ssb <= b, a/(spr), 0),
        "mean"     =a/(spr),
        "dersh"    =ssb*a*(1-b*c*ssb)^c,
        "pellat"   =1/(a/ssb-a/ssb*(ssb/b)^c),
        "bevholtD" =a*(spr)-b,
        "rickerD"  =log(a*spr)/b,
        "shepherdD"=b(a*(spr)-1)^(1/c))}

srrFunc<-function(type,ssb=NULL,spr=NULL,a=NULL,b=NULL,c=NULL,d=NULL){
    #recruits as function of ssb or ssb/red
    if (is.null(ssb) & !is.null(spr))
       ssb<-sprFunc(type,spr,a,b,c)

      switch(type,
          "bevholt"  =a*ssb/(b+ssb),
          "ricker"   =a*ssb*exp(-b*ssb),
          "cushing"  =a*ssb^b,
          "shepherd" =a*ssb/(1+(ssb/b)^c),
          "segreg"   =ifelse(ssb <= b, a*ssb, a*b),
          "mean"     =a,
          "dersh"    =a*(1-b*c*ssb)^(1/c),
          "pellat"   =a*(1-(ssb/b)^c),
          "bevholtD" =a*ssb/(b+ssb),
          "rickerD"  =a*ssb*exp(-b*ssb),
          "shepherdD"=a*ssb/(1+(ssb/b)^c))}

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
        "bevholt" ={a<-(v+(v-s*v)/(5*s-1))/spr0; b<-(v-s*v)/(5*s-1)},
        "ricker"  ={b<-log(5*s)/(v*0.8);         a<-exp(v*b)/spr0},
        "cushing" ={b<-5*log(s);                 a<-(1/spr0)*v^(b-1)}
        )

        res<-c(a,b)
        names(res)<-c("a","b")
        return(res)}

modelFunc<-function(type){
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
        "shepherdD"=rec~a*ssb/(1+(ssb/b)^c))}

loglFunc<-function(type){
    # Loglikelihoods
    switch(type,
        "normlog"   ="sum(dnorm(rec, hat, sigma, TRUE), na.rm=TRUE)",
        "normal"    ="sum(dnorm(rec, hat, sigma, FALSE), na.rm=TRUE)",
        "lognorm"   ="logl.ar1(0,  sigma,rec,hat)",
        "normar1"   ="logl.ar1(rho,sigma,rec,hat)",
        "lognormar1"="logl.ar1(rho,sigma,rec),hat)")}

initialFunc<-function(type){
   # Initial parameter values and upper & lower limits
   switch(type,
        "bevholt"=structure(
               ## initial parameter values
               function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

          			  return(list(a=a, b=b))},

               ## bounds
               lower=c(0, 10e-8),
        	     upper=rep(Inf, 2)),

        "ricker"=structure(
               function(rec, ssb){
        		 	   x    <-ssb
        			   y    <-log(rec/ssb)
                 res  <-coefficients(lm(c(y)~c(x)))

                 a<-max(exp(res[1]))
                 b<--max(res[2])

        			   return(list(a=a, b=b))},

        	  lower=rep(1e-8, 2),
        	  upper=c(Inf,Inf)),

        "cushing"=structure(
               function(rec, ssb){
          		    a <- mean(rec/ssb)
          		    b <- 1.0

                  return(list(a=a, b=b))},

               lower=c(0, 0.0001),
               upper=rep(Inf, 2)),

        "shepherd"=structure(
                function(rec, ssb){
                  a <- mean(rec/ssb,na.rm=T)
                  b <- mean(ssb,na.rm=T)
                  c <- 1.0

                  return(list(a=a,b=b,c=c))},

                lower = c(0,  1e-08, 1),
                upper = c(Inf,Inf,   4)),

       "segreg"=structure(
                function(rec, ssb){
                  a <- median(c(rec/ssb),na.rm=T)
                  b <- median(c(ssb)    ,na.rm=T)

                  return(list(a=a, b=b))},

                  lower=c(0,    0),
                  upper=c(Inf,Inf)
                  #lower=c(exp(mean(log(rec),na.rm=T))/max(ssb,na.rm=T),0)  ,
                  #upper=c(exp(mean(log(rec),na.rm=T))/min(ssb,na.rm=T),Inf)
                  ),

        "mean"=structure(
                function(rec, ssb){
            			a<-mean(rec, na.rm=TRUE)

            			return(list(a=a))},

                  lower=0,
            	    upper=Inf),

        "dersh"=structure(
                function(rec, ssb){
              	a<-mean(rec, na.rm=TRUE)
               	b<-mean(rec, na.rm=TRUE)
                c<-1

                return(list(a=a,b=b,c=c))},

                  lower=c(0,0,0),
            	    upper=c(Inf,Inf,Inf)),

        "pellat"=structure(
                function(rec, ssb){
            			a<-mean(rec, na.rm=TRUE)
                  b<-mean(rec, na.rm=TRUE)

            			return(list(a=a,b=b))},

                  lower=c(0,0,0),
            	    upper=c(Inf,Inf,Inf)),

        "bevholtD"=structure(
               ## initial parameter values
               function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

          			  return(list(a=a, b=b))},

               ## bounds
               lower=c(0, 10e-8, 10e-8),
        	     upper=rep(Inf, 3)),

        "rickerD"=structure(
               function(rec, ssb){
        		 	   x    <-ssb
        			   y    <-log(rec/ssb)
                 res  <-coefficients(lm(c(y)~c(x)))

                 a<-max(exp(res[1]))
                 b<--max(res[2])

        			   return(list(a=a, b=b))},

        	  lower=rep(1e-8, 2),
        	  upper=c(Inf,Inf)),

        "shepherdD"=structure(
                function(rec, ssb){
                  a <- mean(rec/ssb,na.rm=T)
                  b <- mean(ssb,na.rm=T)
                  c <- 1.0

                  return(list(a=a,b=b,c=c))},

                lower = c(0,  1e-08, 1),
                upper = c(Inf,Inf,   4)))}

initialOps<-list("a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b,c","s,v,c,spr0","\t\ta=alpha(mdl,s,v,c,spr0=spr0)\n\t\tb=beta( mdl,s,v,c,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a",    "v,spr0",    "\t\ta=alpha(mdl,v,spr0=spr0)\n",
                 "a,b,c","s,v,c,spr0","\t\ta=alpha(mdl,s,v,c,spr0=spr0)\n\t\tb=beta( mdl,s,v,c,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b",  "s,v,spr0",  "\t\ta=alpha(mdl,s,v,spr0=spr0)\n\t\tb=beta( mdl,s,v,spr0=spr0)\n",
                 "a,b,c","s,v,c,spr0","\t\ta=alpha(mdl,s,v,c,spr0=spr0)\n\t\tb=beta( mdl,s,v,c,spr0=spr0)\n")

initialOps<-t(array(initialOps,c(3,length(srr)),list(arg=c("ab","sv","sv2ab"),srr=srr)))

srModel<- function(mdl="bevholt",ll="normlog",sv=FALSE){
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
                       "\t\tres  \t<-",loglFunc(ll),"\n",
                       "\t\treturn(res)}\n",sep="",file="foo.txt")

    source("foo.txt")
    logl<-foo

	 return(list(logl=logl, model=modelFunc(mdl), initial=initialFunc(mdl)))}

bevholt<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb){
       hat. <-log((a*ssb)/(b+ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)
    	 res  <-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE)

    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

    return(list(a=a, b=b))},

  ## bounds
  lower=c(0, 0.0001),
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
     #res<--logl.ar1(0,sigma,obs,hat.)
	   res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  initial <- structure(function(rec, ssb){
			x <- ssb
			y <- log(rec/ssb)

      res<-coefficients(lm(c(y)~c(x)))

      a<-max(exp(res[1]))
      b<--max(res[2])

			return(list(a=a, b=b))},

		# lower and upper limits for optim()
		lower=rep(1e-8,2),
		upper=c(Inf,Inf))

	model  <- rec~a*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}

segreg <- function(){
	logl <- function(a, b, rec, ssb){
     hat. <-log(ifelse(ssb <= b, a*ssb, a*b))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #res<--logl.ar1(0,sigma,obs,hat.)
	   res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  model <- rec ~ FLQuant(ifelse(ssb <= b, a*ssb, a*b))

  initial <- structure(function(rec, ssb)
    {
    a <- median(c(rec/ssb),na.rm=T)
    b <- median(c(ssb)    ,na.rm=T)

    return(list(a=a, b=b))},

    lower=c(0,0)  ,
    upper=c(Inf,Inf)
    #lower=c(exp(mean(log(rec(s.)),na.rm=T))/max(ssb(s.),na.rm=T),0)  ,
    #upper=c(exp(mean(log(rec(s.)),na.rm=T))/min(ssb(s.),na.rm=T),Inf)
    )

	return(list(logl=logl, model=model, initial=initial))}

shepherd<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a,b,c,rec,ssb){
     hat. <-log(a*ssb/(1+(ssb/b)^c))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #res<--logl.ar1(0,sigma,obs,hat.)
	   res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  ## initial parameter values
    initial<-structure(
      function(rec,ssb){
        a<-mean(rec/ssb,na.rm=T)
        b<-mean(ssb,na.rm=T)
        c<-1.0

        return(list(a=a, b=b, c=c))},

      lower = c(0, 1e-08, 1),
      upper = c(Inf,Inf,4))

  ## model to be fitted
  model <-rec~a*ssb/(1+(ssb/b)^c)

	return(list(logl=logl,model=model,initial=initial))}

pellat<-function(){

  ## log likelihood, assuming normal log.
  logl <- function(a, b, c, rec, ssb){
       hat. <-log(qmax(a*ssb*(1-(ssb/b)^c),0.001))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)
    	 res  <-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE)

    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
      a<-quantile(c(rec/ssb),.9)
      b<-quantile(c(ssb)    ,.9)

    return(list(a=a, b=b, c=1))},

  ## bounds
  lower=c(0, 0, 0),
	upper=rep(Inf, 3))

  ## model to be fitted
  model  <- rec~a*ssb*(1-(ssb/b)^c)


	return(list(logl=logl, model=model, initial=initial))}

dersch<-function(){

  ## log likelihood, assuming normal log.
  logl <- function(a, b, c, rec, ssb){
       hat. <-log(a*(1-b*c*ssb)^(1/c))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)
    	 res  <-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE)

    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
      res<-coefficients(lm(c(rec)~c(ssb)))

      a<-res[1]
      b<--res[2]/a

      return(list(a=a, b=b, c=1))},

  ## bounds
  lower=c(0, 0, 0),
	upper=rep(Inf, 3))

  ## model to be fitted
  model  <- rec~a*(1-b*c*ssb)^(1/c)

	return(list(logl=logl, model=model, initial=initial))}

shepherd.d<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a,b,c,rec,ssb){
     hat. <-log(a*ssb^2/(1+(ssb/b)^c))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #res<--logl.ar1(0,sigma,obs,hat.)
	   res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  ## initial parameter values
    initial<-structure(
      function(rec,ssb){
        a<-mean(rec/ssb^2,na.rm=T)
        b<-mean(ssb,na.rm=T)
        c<-1.0

        return(list(a=a, b=b, c=c))},

      lower = c(0, 1e-08, 1),
      upper = c(Inf,Inf,4))

  ## model to be fitted
  model <-rec~a*ssb^2/(1+(ssb/b)^c)

	return(list(logl=logl,model=model,initial=initial))}

shepherd.ndc<-function(){
    logl <- function(a, b, c, d, rec, ssb){
      ssb<-ssb*(1+d)
      hat. <-log(a*ssb^2/(1+(ssb/b)^c))
      obs  <-log(rec)
	    sigma<-sigma(obs,hat.)

	    # minus log-likelihood
      #res<--logl.ar1(0,sigma,obs,hat.)
	    res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

      if (!is.finite(res)) res<--10e-200

      return(res)}

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0

        return(list(a=a,b=b,c=c,d=1))
        },

      lower = c(0,1e-08,1,0.9), upper = c(Inf,Inf,4,1.1)
      )

    model <- rec ~ a * (ssb*(1+d))/(1 + ((ssb*(1+d))/b)^c)

    return(list(logl = logl, model = model, initial = initial))}

shepherd.ar1<-function(){
   logl <- function(a,b,c,rho,rec,ssb){
     hat. <-log(a*ssb/(1+(ssb/b)^c))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     res<--logl.ar1(rho,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

    initial <- structure(function(rec, ssb){
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        return(list(a=a,b=b,c=c,rho=rho))},

      lower = c(0, 1e-08, 1, -0.9),
      upper = c(Inf,Inf,4,0.9))

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))}

shepherd.d.ar1<-function(){
  logl <- function(a,b,c,rho, rec, ssb){
     hat. <-log(a*ssb^2/(1+(ssb/b)^c))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     res<--logl.ar1(rho,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

    initial <- structure(function(rec, ssb){
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        return(list(a = a, b = b, c = c, rho=rho))},

      lower = c(0, 1e-08, 1, -0.9, 1e-08),
      upper = c(Inf,Inf,4,0.9,Inf))

    model <- rec ~ a * ssb^2/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))}

shepherd.ndc.ar1<-function(){
    logl <- function(a,b,c,d,rho,rec,ssb){
       ssb  <-ssb*(1+d)
       hat. <-log(a*(ssb-d)/(1+((ssb-d)/b)^c))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

	     # minus log-likelihood
       res<--logl.ar1(rho,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0

        return(list(a = a, b = b, c = c, d=0, rho=0))
        },

      lower = c(0, 1e-08, 1, -1.5, -0.9), upper = c(Inf,Inf,4,0.5,0.9)
      )

    model <- rec ~ a * (ssb-d)/(1 + ((ssb-d)/b)^c)

    return(list(logl = logl, model = model, initial = initial))}
    
bevholt.d<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, c, rec, ssb){
       hat. <-log((a*ssb^c)/(b+ssb^c))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

	     # minus log-likelihood
       #res<--logl.ar1(0,sigma,obs,hat.)
	     res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

       if (!is.finite(res)) res<- -10e-200

       return(res)}

    ## initial parameter values
    initial <- structure(function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

       return(list(a=a,b=b,c=1))},

  ## bounds
  lower=c(  0,   0, 0.5),
	upper=c(Inf, Inf, 2.0))

  ## model to be fitted
  model<-rec~(a*ssb^c)/(b+ssb^c)

	return(list(logl=logl, model=model, initial=initial))}

ricker.d<-function(){
	logl <- function(a, b, c, rec, ssb){
       hat. <-log(a*ssb^c*exp(-b*ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

	     # minus log-likelihood
       #res<--logl.ar1(0,sigma,obs,hat.)
	     res<-sum(dnorm(obs, hat., sigma, TRUE), TRUE)

       if (!is.finite(res)) res<- -10e-200

       return(res)}

  initial <- structure(function(rec, ssb){
			x <- ssb
			y <- log(rec/ssb^2)

      res<-coefficients(lm(c(y)~c(x)))

      a<-max(exp(res[1]))
      b<--max(res[2])

			return(list(a=a, b=b, c=1))},

		# lower and upper limits for optim()
		lower=c(1e-8,1e-8, 0.5),
		upper=c( Inf, Inf, 1.0))

	model  <- rec~a*ssb^c*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}

RickerSV <- function (s, v, spr0, ssb){
  b <- log(5*s)/(v*0.8)
  a <- exp(b*v)/spr0
  
  return(a*ssb*exp(-b*ssb))}

rickerSV <- function(){
  logl<-function(s,v,spr0,rec,ssb){
     sigma<-sigma(log(rec), log(BevholtSV(s,v,spr0,ssb)))
     sum(dnorm(log(rec), log(RickerSV(s,v,spr0,ssb)),sigma,TRUE),na.rm=TRUE)}

  initial <- structure(function(rec,ssb) {
     return(list(s=.5,v=mean(as.vector(ssb))*2,spr0=.5))},

     lower = c(.1, 1e-08, 1e-08),
     upper = c(5, rep(Inf, 2)))

  model<-rec~RickerSV(s,v,spr0,ssb)

  return(list(logl=logl,model=model,initial=initial))}

BevholtSV<-function(s,v,spr0,ssb){
    param<-sv2ab(s,v,spr0,"bevholt")

    return(param["a"]*ssb/(param["b"]+ssb))}

bevholtSV<-function(){
    logl <- function(s,v,spr0,rec,ssb){
       sigma<-sigma(log(rec),log(BevholtSV(s,v,spr0,ssb)))
       sum(dnorm(log(rec),log(BevholtSV(s,v,spr0,ssb)),sigma,TRUE),na.rm=TRUE)}

    initial<-structure(function(rec,ssb){
        return(list(s=.75,v=mean(as.vector(ssb),na.rm=TRUE)*2,spr0=1))},

        lower = c(.21,rep(1e-08, 3)),
        upper = c(1,rep(Inf, 3)))

    model<-rec~BevholtSV(s,v,spr0,ssb)

    return(list(logl=logl,model=model,initial=initial))}
