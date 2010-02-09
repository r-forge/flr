

bevholt<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb){
       hat. <-log((a*ssb)/(b+ssb))
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)
    	 return(res)}

  ## initial parameter values
  initial <- structure(function(rec, ssb){
          		 	  a <- max(quantile(c(rec), .75, na.rm=TRUE))
          		  	b <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))
                  b<-a/b

    return(list(a=a,b=b))},

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

cushing<-function(){
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb){
       hat. <-log(a*ssb^b)
       obs  <-log(rec)
	     sigma<-sigma(obs,hat.)

       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)
    	 return(res)}

  ## initial parameter values
  initial<-structure(function(rec, ssb){
       a <- mean(rec/ssb)
       b <- 1.0

       res<-list(a=a,b=b)

       return(res)},

  ## bounds
  lower=c(0, 0.0001),
	upper=rep(Inf, 2))


  ## model to be fitted
  model  <- rec~a*ssb^b

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

shepherd<-function(){
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

geomean<-function(){
    logl <- function(a,rec,ssb){
       hat. <-log(a)
       obs  <-log(rec)
  	   sigma<-sigma(obs,hat.)

  	   # minus log-likelihood
       #if (flagNormLog) res<-sum(dnorm(obs, hat., sigma, TRUE), na.rm=TRUE) else
                        res<-loglAR1(obs,hat.,0,sigma)

       if (!is.finite(res)) res<--10e-200

       return(res)}

    initial <- structure(function(rec) {
        a     <- exp(mean(log(rec), na.rm=TRUE))
        sigma2 <- var(log(rec/a), na.rm = TRUE)
        return(list(a = a, sigma2 = sigma2))
        },
        lower = c(1e-08, 1e-08), upper = rep(Inf, 2))

    model <- rec ~ a

    return(list(logl = logl, model = model, initial = initial))}

#### Backwards compatibility
ricker.d    <-function(mdl="rickerD"    ,ll="normlog",   sv=FALSE)           srModel(mdl,ll,sv)
ricker.c.a  <-function(mdl="ricker"     ,ll="normlog",   sv=FALSE, ca=T)     srModel(mdl,ll,sv,ca=ca)
ricker.c.b  <-function(mdl="ricker"     ,ll="normlog",   sv=FALSE, cb=T)     srModel(mdl,ll,sv,cb=cb)
ricker.sv   <-function(mdl="ricker"     ,ll="normlog",   sv=TRUE)            srModel(mdl,ll,sv)
ricker.ar1  <-function(mdl="ricker"     ,ll="lognormar1",sv=FALSE)           srModel(mdl,ll,sv)
bevholt.ar1 <-function(mdl="bevholt"    ,ll="lognormar1",sv=FALSE)           srModel(mdl,ll,sv)
bevholt.d   <-function(mdl="bevholtD"   ,ll="normlog",   sv=FALSE)           srModel(mdl,ll,sv)
bevholt.c.a <-function(mdl="bevholt"    ,ll="normlog",   sv=FALSE, ca=T)     srModel(mdl,ll,sv,ca=ca)
bevholt.c.b <-function(mdl="bevholt"    ,ll="normlog",   sv=FALSE, ca=T)     srModel(mdl,ll,sv,cb=cb)
bevholt.sv  <-function(mdl="bevholt"    ,ll="normlog",   sv=TRYE)            srModel(mdl,ll,sv)
bevholt.ndc <-function(mdl="bevholt"    ,ll="normlog",   sv=FALSE, ndc=TRUE) srModel(mdl,ll,sv,ndc=ndc)
shepherd.ar1<-function(mdl="shepherd"   ,ll="lognormar1",sv=FALSE)           srModel(mdl,ll,sv)
shepherd.d  <-function(mdl="shepherdD"  ,ll="normlog",   sv=FALSE)           srModel(mdl,ll,sv)


