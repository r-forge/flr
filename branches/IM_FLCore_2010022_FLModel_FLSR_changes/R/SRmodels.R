# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, Cefas
# $Id$

# Reference:
# Notes:

# models

# ricker  {{{
ricker <- function()
{
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb*exp(-b*ssb)), sigma(log(rec), log(a*ssb*exp(-b*ssb)))^2)

  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(list(a=max(exp(res[1])), b=-max(res[2])))
	},
  # lower and upper limits for optim()
	lower=rep(1e-10, 2),
	upper=rep(Inf, 2)
	)
	model  <- rec~a*ssb*exp(-b*ssb)
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholt {{{
bevholt <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb/(b+ssb)), sigma(log(rec), log(a*ssb/(b+ssb)))^2)

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(list(a = a, b = a/b))
	},

  ## bounds
  lower=rep(10e-8, 2),
	upper=rep(Inf, 2))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# segreg  {{{
segreg <- function()
{
	logl <- function(a, b, rec, ssb)
    loglAR1(log(rec), log(ifelse(ssb<=b,a*ssb,a*b)), sigma(log(rec), log(ifelse(ssb<=b,a*ssb,a*b)))^2)

  model <- rec ~ FLQuant(ifelse(ssb<=b,a*ssb,a*b))

  initial <- structure(function(rec, ssb)
  {
    return(list(a=median(c(rec/ssb)), b=median(c(ssb))))
  },
    lower=rep(0, 1e-7),
    upper=rep(Inf, 2))

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# geomean {{{
geomean<-function() 
    {
    logl <- function(a, rec)
      loglAR1(log(rec), log(FLQuant(rep(a, length(rec)))), sigma(log(rec), log(FLQuant(rep(a, length(rec)))))^2)
    
    initial <- structure(function(rec) {
        return(list(a = exp(mean(log(rec), na.rm=TRUE))))
        }, 
        lower = c(1e-08), upper = rep(Inf))
    
    model <- rec ~ FLQuant(a, dimnames=dimnames(rec))
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# shepherd  {{{
shepherd <- function()
{
  logl <- function(a,b,c,rec,ssb)
      loglAR1(log(rec), log(a*ssb/(1+(ssb/b)^c)), sigma(log(rec), log(a*ssb/(1+(ssb/b)^c)))^2)

  initial <- structure(function(rec,ssb)
  {
    c <- 1
    a <- quantile(c(rec/ssb),prob=0.75,na.rm=T, names=FALSE)
    b <- (quantile(c(rec),prob=0.75,na.rm=T, names=FALSE)*quantile(c(ssb^(c-1)),
      prob=0.5,na.rm=T, names=FALSE)/a)^(1/c)
    return(list(a=a,b=b,c=c))
  },
    lower = c(1e-08, 1e-08, 1),
    upper = c(1e02,  1e+08,10))

  model <- rec ~ a * ssb/(1 + (ssb/b)^c)

  return(list(logl = logl, model = model, initial = initial))
} # }}}

# cushing {{{
cushing<-function()
{
  logl <- function(a, b, rec, ssb)
    loglAR1(log(rec), log(a*ssb^b), sigma(log(rec), log(a*ssb^b))^2)

  initial <- structure(function(rec, ssb)
  {
    a <- mean(rec/ssb)
    b <- 1.0
    return(list(a=a,b=b))
  },
  lower=c(0, 0.0001),
	upper=c(Inf, 1))

  model  <- rec~a*ssb^b

	return(list(logl=logl, model=model, initial=initial))
}  # }}}

# rickerSV  {{{
rksv2ab <- function(s, v, spr0)
{
  b <- log(5.0*s)/(v*0.8)
  a <- exp(b*v)/spr0
  return(unlist(list(a=a, b=b)))
}
rkab2sv <- function(a,b,spr0)
{
  v <- log(spr0 * a)/b
	s <- 0.2*exp(b*(v)*0.8)
  return(unlist(list(s=s, v=v, spr0=spr0)))
}

rickerSV <- function()
{
  logl <- function(s, v, spr0, rec, ssb)
  { 
    pars <- rksv2ab(s, v, spr0)
    loglAR1(log(rec), log(pars['a']*ssb*exp(-pars['b']*ssb)), sigma(log(rec),
      log(pars['a']*ssb*exp(-pars['b']*ssb)))^2)
  }

  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, rm.na = F, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(list(s=s, v=v, spr0=spr0))
	},
  ## bounds
  lower=c(1e-8, rep(1e-8, 2)),
	upper=c(10, Inf, Inf))

	model  <- rec~rksv2ab(s, v, spr0)['a']*ssb*exp(-rksv2ab(s, v, spr0)['b']*ssb)

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholtSV {{{
bhsv2ab <- function(s, v, spr0)
{
  a <- v*4*s / (spr0*(5*s-1.0))
  b  <- a*spr0*(1.0/s - 1.0)/4.0
  return(unlist(list(a=a, b=b)))
}
bhab2sv <- function(a,b,spr0)
{
  s <- a*spr0/(4*b+a*spr0)
	v <- (spr0*a*(5*s-1))/(4*s)
  return(unlist(list(s=s, v=v, spr0=spr0)))
}
bevholtSV <- function()
  {
  logl <- function(s, v, spr0, rec, ssb)
  {
    pars <- bhsv2ab(s, v, spr0)
    loglAR1(log(rec), log(pars['a']*ssb/(pars['b']+ssb)), sigma(log(rec), log(pars['a']*
      ssb/(pars['b']+ssb)))^2)
  }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, rm.na = F, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(list(s=s, v=v, spr0=spr0))
	},
  ## bounds
  lower=c(0.2, rep(10e-8, 2)),
	upper=c(0.999, Inf, Inf))

  ## model to be fitted
  model  <- rec~bhsv2ab(s, v, spr0)['a']*ssb/(bhsv2ab(s, v, spr0)['b']+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# shepherdSV {{{
shsv2ab <- function(s, v, spr0)
{
  a <- v*4*s / (spr0*(5*s-1.0))
  b  <- a*spr0*(1.0/s - 1.0)/4.0
  return(unlist(list(a=a, b=b, c=)))
}
shab2sv <- function(a,b,spr0)
{
  s <- a*spr0/(4*b+a*spr0)
	v <- (spr0*a*(5*s-1))/(4*s)
  return(unlist(list(s=s, v=v, spr0=spr0)))
}
bevholtSV <- function()
  {
  logl <- function(s, v, spr0, rec, ssb)
  {
    pars <- bhsv2ab(s, v, spr0)
    loglAR1(log(rec), log(pars['a']*ssb/(pars['b']+ssb)), sigma(log(rec), log(pars['a']*
      ssb/(pars['b']+ssb)))^2)
  }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, rm.na = F, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(list(s=s, v=v, spr0=spr0))
	},
  ## bounds
  lower=c(0.2, rep(10e-8, 2)),
	upper=c(0.999, Inf, Inf))

  ## model to be fitted
  model  <- rec~bhsv2ab(s, v, spr0)['a']*ssb/(bhsv2ab(s, v, spr0)['b']+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# methods

# spr0  {{{
## calcs spawner per recruit at F=0.0   
setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
   function(ssb, rec, fbar)
  {
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
    spr0 <- lm(c(ssb/rec)~c(fbar))$coefficients[1]
    names(spr0) <- "spr0"

    return(spr0)
  }
)

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb)
  {
    # rec
    sr <- as.FLSR(ssb)

    # spr0
    spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb))
  }
)

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar)
  {
    # spr0
    spr0(ssb=ssb(ssb), rec=rec(ssb), fbar=fbar)
  }
)
# }}}

# SRModelName {{{
SRModelName<-function(formula)
{
  srmodels <- list('ricker', 'ricker.d', 'ricker.c.a', 'ricker.c.b', 'ricker.sv',
  'ricker.ar1', 'bevholt', 'bevholt.ar1','bevholt.d', 'bevholt.c.a', 'bevholt.c.b',   
  'bevholt.sv', 'bevholt.ndc', 'shepherd', 'shepherd.ar1', 'shepherd.d', 'geomean', 'segreg')
  srformulae <- lapply(srmodels, function(x) do.call(x, list())$model)
  names(srformulae) <- srmodels
  for(i in srmodels)
    if(formula == srformulae[[i]])
      return(i)
  return(FALSE)
} # }}}

# rSq {{{
setMethod('rSq', signature(obs='FLQuant',hat='FLQuant'),
  function(obs,hat=rep(0,length(obj)))
  {
    ## calculates R squared
    mn   <-mean(obs)
    mnHat<-mean(hat)
    SStot<-sum((obs-mn)^2)
    SSreg<-sum((hat-mnHat)^2)
    SSerr<-sum((obs-hat)^2)

    res  <-1-SSerr/SStot

    return(res)
  }
) # }}}

# loglAR1 {{{
setMethod('loglAR1', signature(obs='FLQuant', hat='FLQuant'),
  function(obs, hat, sigma2=sigma(obs, hat) ^ 2, rho=0)
  {
    # calculates likelihood for AR(1) process
    n <- length(obs)
    s2 <- sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
    s1 <- (1-rho^2)*(obs[,1]-hat[,1])^2 + s2
    sigma2.a <- (1-rho^2)*sigma2
    res <- (log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    if (!is.finite(res)) 
      res <- -1e100

    return(res)
  }
) # }}}

# spr2ssb {{{
spr2ssb <- function(model, params)
{
  call <- switch(as.character(model)[3],
    # bevholt
    "a * ssb/(b + ssb)" = expression(a*spr-b),
    # ricker
    "a * ssb * exp(-b * ssb)" = expression(log(a*spr)/b),
    # cushing
    "a * ssb^b" = expression((1/(a*spr))^(1/(b-1))),
    # shepherd
    "a * ssb/(1 + (ssb/b)^c)" = expression(b*(a*spr-1)^(1/c)),
    )
  FLPar(eval(call, params), params='ssb', iter=max(unlist(lapply(params, length))))
} # }}}

# srmodel

# Depensatory Ricker {{{
ricker.d<-function () 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * (ssb^c) * exp(-b * ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(function(rec, ssb) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 1
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a * 
            (ssb^c) * exp(-b * ssb)))))
    }, lower = c(1e-08, 1e-08,1e-08, 1e-08), upper = rep(Inf, 4))
    
    model <- rec ~ a * (ssb^c) * exp(-b * ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    }   # }}}

# Ricker with covariate  {{{
ricker.c.a<-function () 
  {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*(1-c*covar) * ssb * exp(-b * ssb)), sqrt(sigma2),
        TRUE), na.rm=TRUE)
    initial <- structure(function(rec, ssb, covar) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 0
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a*(1-c*covar) * 
            ssb * exp(-b * ssb)))))
    }, lower = c(1e-08, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4))
    model <- rec ~ a*(1-c*covar) * ssb * exp(-b * ssb)
    return(list(logl = logl, model = model, initial = initial))
  }

ricker.c.b<-function () 
  {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a * ssb * exp(-b*(1-c*covar) * ssb)), sqrt(sigma2),
        TRUE), na.rm=TRUE)
    initial <- structure(function(rec, ssb, covar) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 0
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a * 
            ssb * exp(-b*(1-c*covar) * ssb)))))
    }, lower = c(1e-08, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4))
    model <- rec ~ a * ssb * exp(-b*(1-c*covar) * ssb)
    return(list(logl = logl, model = model, initial = initial))
  } # }}}

# Ricker parameterised for steepness & virgin biomass {{{
Ricker.SV <- function (steepness, vbiomass, spr0, ssb) 
{
  b <- log(5 * steepness) / (vbiomass * 0.8)
  a <- exp(b * vbiomass) / spr0
  return(a * ssb * exp(-b* ssb))      
}

ricker.sv <- function()
{
  logl <- function(steepness, vbiomass, spr0, sigma2, rec, ssb)
    sum(dnorm(log(rec), log(Ricker.SV(steepness, vbiomass, spr0, ssb)), sqrt(sigma2),
    TRUE), na.rm=TRUE)

  initial <- structure(function(rec, ssb) {
    return(list(steepness = .5, vbiomass = mean(as.vector(ssb))*2, spr0=spr0,
      sigma2 = 0.3))
  }, lower = c(.1, 1e-08, 1e-08, 1e-08), upper = c(5, rep(Inf, 3)))

  model <- rec ~ Ricker.SV(steepness, vbiomass, spr0, ssb)

  return(list(logl = logl, model = model, initial = initial))
} # }}}

# Bevholt {{{
bevholt.d<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb^c/(b + ssb^c)), sqrt(sigma2), TRUE), na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb^c/(b + ssb^c))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(0, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4)
      )
    
    model <- rec ~ a * ssb^c/(b + ssb^c)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Bevholt NDC {{{
bevholt.ndc<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb*(1+c)/(b + ssb*(1+c))), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 0.0
        sigma2 <- var(log(rec/(a * ssb*(1+c)/(b + ssb*(1+c)))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -0.5, 1e-08), upper = c(Inf,Inf,0.5,Inf)
      )
    
    model <- rec ~ a * ssb*(1+c)/(b + ssb*(1+c))
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Bevholt covariates {{{
bevholt.c.b<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*ssb/(b*(1-c*covar) + ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb, covar) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb)
        c <- 0.0
        sigma2 <- var(log(rec/(a * (1-c*covar)*ssb/(b + ssb))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -10, 1e-08), upper = rep(Inf, 4)
      )
    
    model <- rec ~ a*ssb/(b*(1-c*covar) + ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    }

bevholt.c.a<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*(1-c*covar) * ssb/(b + ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb, covar) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 0.0
        sigma2 <- var(log(rec/(a*(1-c*covar) * ssb/(b + ssb))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -1, 1e-08), upper = c(Inf,Inf,1,Inf)
      )
    
    model <- rec ~ a*(1-c*covar) * ssb/(b + ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}
    
# bevholt parameterised for steepness & virgin biomass  {{{
Bevholt.SV<-function(s,v,spr0,ssb)
    {
    param<-sv2ab(s,v,spr0,"bevholt")

    return(param["a"] * ssb/(param["b"] + ssb))
    }
    
bevholt.sv<-function()
    {
    logl <- function(s, v, spr0, sigma2, rec, ssb)
       sum(dnorm(log(rec), log(Bevholt.SV(s,v,spr0,ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)

    initial <- structure(function(rec, ssb) {
        return(list(s = .75, v = mean(as.vector(ssb), na.rm=TRUE)*2, spr0=1,
          sigma2 = 0.3))
    }, lower = c(.21,rep(1e-08, 3)), upper = c(1,rep(Inf, 3)))

    model <- rec ~ Bevholt.SV(s,v,spr0,ssb)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# logl.ar1  {{{
logl.ar1<-function(rho,sigma2,obs,hat)
  {
  n        <-length(obs)
  s2       <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
  s1       <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
  sigma2.a <-(1-rho^2)*sigma2
  res      <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

  return(res)
  } # }}}

# bevholt AR1  {{{
bevholt.ar1 <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rho, sigma2, rec, ssb)
     {
     hat<-a*+ssb/(b+ssb)

     return(logl.ar1(rho,sigma2,log(rec),log(hat)))
     }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
	  	{
			a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
			b <- 0.5 * min(ssb, na.rm=TRUE)
      rho<-0.0
			sigma2 <- var(log(rec /( a * ssb / (b + ssb))), y= NULL, na.rm = TRUE)
			return(list(a=a, b=b, rho=rho, sigma2=sigma2))
  		},

  ## bounds
  lower=c(0, 1e-8, -0.5, 1e-8),
	upper=c(Inf,Inf,0.5,Inf))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)

	return(list(logl=logl, model=model, initial=initial))
} #}}}

# Ricker AR1  {{{
ricker.ar1<-function()
    {
    logl <- function(a, b, rho, sigma2, rec, ssb) 
       {
       hat<-a*log(ssb)*exp(-b*log(ssb))
           
       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }
    
    initial <- structure(function(rec, ssb) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        rho<-0
    
        return(list(a = a, b = b, rho=rho, sigma2 = var(log(rec) - log(a * 
            ssb * exp(-b * ssb)))))
        }, 
        lower = c(1e-08, 1e-08, -1.0, 1e-08), upper = c(Inf,Inf,1.0,Inf))
    
    model <- rec ~ a * ssb * exp(-b * ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

shepherd <- function(){
  logl <- function(a,b,c,rec,ssb)
      loglAR1(log(rec), log(a*ssb/(1+(ssb/b)^c)), sigma(log(rec), log(a*ssb/(1+(ssb/b)^c)))^2)

  initial <- structure(function(rec,ssb){
        c <- 2
    		x <- ssb^c
		    y <- ssb/rec

        res<-coefficients(lm(c(y)~c(x)))

        a<-max(1/res[1])
        b<-max(b=1/((res[2]*a)^(1/c)))

    return(list(a=a,b=b,c=c))},
    
    lower = c(1e-08, 1e-08, 1),
    upper = c(1e02,  1e+08,10))

  model <- rec ~ a * ssb/(1 + (ssb/b)^c)

  return(list(logl = logl, model = model, initial = initial))}

# Sheperd {{{
shepherd.d<-function()
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb^2/(1 + (ssb/b)^c)), sqrt(sigma2), TRUE), na.rm=TRUE)

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb^2/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, 1e-08), upper = c(Inf,Inf,4,Inf)
      )

    model <- rec ~ a * ssb^2/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd {{{
shepherd.ndc<-function()
    {
    logl <- function(a, b, c, d, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * (ssb-d)/(1 + ((ssb-d)/b)^c)), sqrt(sigma2), TRUE), na.rm=TRUE)

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, d=0, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -1.5, 1e-08), upper = c(Inf,Inf,4,0.5,Inf)
      )

    model <- rec ~ a * (ssb-d)/(1 + ((ssb-d)/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.ar1 {{{
shepherd.ar1<-function()
    {
    logl <- function(a,b,c,rho, sigma2, rec, ssb)
       {
       hat<-a * ssb/(1 + (ssb/b)^c)

       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, rho=rho, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -0.9, 1e-08), upper = c(Inf,Inf,4,0.9,Inf)
      )

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.d.ar1 {{{
shepherd.d.ar1<-function()
    {
    logl <- function(a,b,c,rho, sigma2, rec, ssb)
       {
       hat<-a * ssb^2/(1 + (ssb/b)^c)

       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        sigma2 <- var(log(rec/(a * ssb^2/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, rho=rho, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -0.9, 1e-08), upper = c(Inf,Inf,4,0.9,Inf)
      )

    model <- rec ~ a * ssb^2/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.ar1 {{{
shepherd.ndc.ar1<-function()
    {
    logl <- function(a,b,c,d, rho, sigma2, rec, ssb)
       {
       hat<-a * (ssb-d)/(1 + ((ssb-d)/b)^c)
       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, d=0, rho=0, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -1.5, -0.9, 1e-08), upper = c(Inf,Inf,4,0.5,0.9,Inf)
      )

    model <- rec ~ a * (ssb-d)/(1 + ((ssb-d)/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}
