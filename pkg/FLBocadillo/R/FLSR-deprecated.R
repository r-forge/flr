# Here are the functions, classes and methods that people may still be using but will no longer be maintained.
# They are kept for legacy reasons but may be deleted soon.

bevholt<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb){
       hat. <-log((a*ssb)/(b+ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)
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
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

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
     hat. <-FLQuant(log(ifelse(ssb <= b, a*ssb, a*b)))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  model <- rec ~ FLQuant(ifelse(ssb <= b, a*ssb, a*b))

  initial <- structure(function(rec, ssb)
    {
    a <- median(c(rec/ssb),na.rm=T)
    b <- median(c(ssb)    ,na.rm=T)

    return(list(a=a, b=b))},

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
                        res<-loglAR1(0,sigma,obs,hat.)

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

    return(list(logl = logl, model = model, initial = initial))
    } # }}}


pellat<-function(){

  ## log likelihood, assuming normal log.
  logl <- function(a, b, c, rec, ssb){
       hat. <-log(qmax(a*ssb*(1-(ssb/b)^c),0.001))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
      a<-quantile(c(rec/ssb),.9)
      b<-quantile(c(ssb)    ,.9)

    return(list(a=a,b=b,c=1))},

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
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

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

shepherdD<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a,b,c,rec,ssb){
     hat. <-log(a*ssb^2/(1+(ssb/b)^c))
     obs  <-log(rec)
	   sigma<-sigma(obs,hat.)

	   # minus log-likelihood
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  ## initial parameter values
    initial<-structure(
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

      lower = c(0, 1e-08, 1),
      upper = c(Inf,Inf,10))

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
      #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                       res<-loglAR1(0,sigma,obs,hat.)

      if (!is.finite(res)) res<--10e-200

      return(res)}

    initial <- structure(
      function(rec,ssb){
        c<-2
        a<-quantile(c(rec/ssb),prob=0.75,na.rm=T)
        #b<-quantile(c(rec),prob=0.75,na.rm=T)/a

        b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)

        names(a)<-NULL
        names(b)<-NULL

        return(list(a=a, b=b, c=c))
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
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

    initial <- structure(
      function(rec,ssb){
        c<-2
        a<-quantile(c(rec/ssb),prob=0.75,na.rm=T)
        #b<-quantile(c(rec),prob=0.75,na.rm=T)/a

        b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)

        names(a)<-NULL
        names(b)<-NULL

        return(list(a=a,b=b,c=c,rho=0))},

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
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

    initial <- structure(
          function(rec,ssb){
              c<-2
              a<-quantile(c(rec/ssb),prob=0.75,na.rm=T)
              #b<-quantile(c(rec),prob=0.75,na.rm=T)/a

              b<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)

              names(a)<-NULL
              names(b)<-NULL

              return(list(a=a,b=b,c=c,rho=0))
              },

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
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial <- structure(
      function(rec, ssb, c=2)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)

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
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

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
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

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

.rickerSV <- function (s, v, spr0, ssb){
  b <- log(5*s)/(v*0.8)
  a <- exp(b*v)/spr0

  return(a*ssb*exp(-b*ssb))}

rickerSV <- function(){
  logl<-function(s,v,spr0,rec,ssb){
     obs <-log(rec)
     hat.<-log(.rickerSV(s,v,spr0,ssb))
     sigma<-sigma(obs,hat.)

     # minus log-likelihood
     #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                      res<-loglAR1(0,sigma,obs,hat.)

     if (!is.finite(res)) res<--10e-200

     return(res)}

  initial <- structure(function(rec,ssb) {
     return(list(s=.5,v=mean(as.vector(ssb))*2,spr0=.5))},

     lower = c(.1, 1e-08, 1e-08),
     upper = c(5, rep(Inf, 2)))

  model<-rec~.rickerSV(s,v,spr0,ssb)

  return(list(logl=logl,model=model,initial=initial))}

.bevholtSV<-function(s,v,spr0,ssb){
    param<-sv2ab(s,v,spr0,"bevholt")

    res<-param["a",1]*ssb/(param["b",1]+ssb)

    return(res)}

bevholtSV<-function(){
    logl <- function(s,v,spr0,rec,ssb){
       obs <-log(rec)
       hat.<-log(.bevholtSV(s,v,spr0,ssb))

       sigma<-sigma(obs,hat.)

       # minus log-likelihood
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial<-structure(function(rec,ssb){
        spr0=quantile(c(ssb/rec),prob=0.9,rm.na=F)
        names(spr0)<-NULL
        return(list(s=.75,v=mean(as.vector(ssb),na.rm=TRUE)*2,spr0=spr0))},

        lower = c(.21,rep(1e-08, 3)),
        upper = c(1,rep(Inf, 3)))

    model<-rec~.bevholtSV(s,v,spr0,ssb)

    return(list(logl=logl,model=model,initial=initial))}

.shepherdSV<-function(s,v,c,spr0,ssb){
    param<-ab("shepherd",spr0=spr0,s=s,v=v,c=c)
    
#    return(a*ssb/(1+(ssb/b)^c))}
    return(srrFunc("shepherd",ssb=ssb,a=param["a"],b=param["b"],c=c))}

shepherdSV<-function(){
    logl <- function(s,v,c,spr0,rec,ssb){

       hat.<- log(.shepherdSV(s,v,c,spr0,ssb))
       obs <- log(rec)

       sigma<-sigma(obs,hat.)

       # minus log-likelihood
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(0,sigma,obs,hat.)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial<-structure(
      function(rec,ssb){

        spr0=quantile(c(ssb/rec),prob=0.9,rm.na=F)
        v   =quantile(c(ssb),prob=0.90,na.rm=T)
        names(spr0)<-NULL
        names(v)   <-NULL

        return(list(s=0.75,v=v,c=2,spr0=spr0))
        },

        lower = c(.21,1e-08,1,1e-08),
        upper = c(0.999,Inf,10,Inf))

    model<-rec~.shepherdSV(s,v,c,spr0,ssb)

    return(list(logl=logl,model=model,initial=initial))}


