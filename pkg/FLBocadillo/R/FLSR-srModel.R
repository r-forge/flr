# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT

# Reference:
# Notes: Updated to make generic

flagNormLog=FALSE
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

loglAR1<-function(rho,sigma,obs,hat){
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
        "shepherd" =b*(a*spr-1)^(1/c),
        "segreg"   =ifelse(ssb <= b, a/(spr), 0),
        "mean"     =a/(spr),
        "dersh"    =ssb*a*(1-b*c*ssb)^c,
        "pellat"   =1/(a/ssb-a/ssb*(ssb/b)^c),
        "bevholtD" =a*(spr)-b,
        "rickerD"  =log(a*spr)/b,
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
          "segreg"   =ifelse(ssb <= b, a*ssb, a*b),
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
        "bevholt" ={a<-(v+(v-s*v)/(5*s-1))/spr0; b<-(v-s*v)/(5*s-1)},
        "ricker"  ={b<-log(5*s)/(v*0.8);         a<-exp(v*b)/spr0},
        "cushing" ={b<-5*log(s);                 a<-(1/spr0)*v^(b-1)},
        "segreg"  =stop("can´t parameterise segmented regression as steepness and virgin biomass"),
        "shepherd"={b<-v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c));   a<-((v/b)^c+1)/spr0})

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
        "shepherdD"=rec~a*ssb^2/(1+(ssb/b)^c))}

loglFunc<-function(type){
    # Loglikelihoods
    switch(type,
        "normlog"   ="sum(dnorm(rec, hat, sigma, TRUE), na.rm=TRUE)",
        "normal"    ="sum(dnorm(rec, hat, sigma, FALSE), na.rm=TRUE)",
        "lognorm"   ="loglAR1(0,  sigma,rec,hat)",
        "normar1"   ="loglAR1(rho,sigma,rec,hat)",
        "lognormar1"="loglAR1(rho,sigma,rec),hat)")}

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
                function(rec,ssb){
                  c<-2
                  a<-quantile(c(rec/ssb),prob=0.75,na.rm=T)
                  #b<-quantile(c(rec),prob=0.75,na.rm=T)/a

                  b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)

                  names(a)<-NULL
                  names(b)<-NULL

                  return(list(a=a, b=b, c=c))
                  },

                lower = c(0,  1e-08, 1),
                upper = c(Inf,Inf,   4)),

       "segreg"=structure(
                function(rec, ssb){
                  a <- median(c(rec/ssb),na.rm=T)
                  b <- median(c(ssb)    ,na.rm=T)

                  return(list(a=a, b=b))},

                  lower=c(1e-8, 0),
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
                function(rec,ssb){
                  a <- mean(rec/ssb,na.rm=T)
                  b <- mean(ssb,na.rm=T)/a
                  c <- 1.0

                  return(list(a=a, b=b, c=c))
                  },

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

