# FLSP - Class and methods for Surplus Production models
# FLAssess/R/FLSP.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: FLSP.R,v 1.4 2008/06/20 11:42:30 imosqueira Exp $

# class FLSP {{{
setClass('FLSP', representation(
  'FLModel',
  catch='FLQuant',
  index='FLQuant',
  mpar='numeric',
  delta='numeric')
) # }}}

# FLSP()	{{{
setGeneric('FLSP', function(model, ...)
		standardGeneric('FLSP'))
setMethod('FLSP', signature(model='ANY'),
  function(model, ...)
    return(FLModel(model, ..., class='FLSP')))
setMethod('FLSP', signature(model='missing'),
	function(...)
		return(FLModel(formula(NULL), ..., class='FLSP'))) # }}}

# PellaTom {{{
PellaTom <- function(catch, r, K, Q, mpar, delta)
{
  dm <- dimnames(catch)
  catch <- as.vector(catch)
  biomass <- rep(delta, length(catch))
  for(y in seq(2, length(catch)))
    biomass[y] <- biomass[y-1] + r * biomass[y-1] * (1 - biomass[y-1] ^ (mpar-1)) -
      catch[y-1]/K
  biomass[biomass <= 0] <- 1e-9
 return(FLQuant(Q*biomass, dimnames=dm))
}

pellatom <- function()
{
  logl <- function(Q, r, K, sigma2, mpar, delta, catch, index)
  {
    sum(dnorm(log(index), window(log(PellaTom(catch, r, K, Q, mpar, delta)),
      start=dims(index)$minyear,end=dims(index)$maxyear), sqrt(sigma2), TRUE), na.rm=TRUE)
  }
  initial <- structure(function(catch) return(TRUE),
    lower=rep(1e-6, 4), upper=rep(Inf, 4))
  return(list(model=index~PellaTom(catch, r, K, Q, mpar, delta), logl=logl, 
    initial=initial))
} # }}}

# PellaTomC {{{
pellatomC <- function()
{
  logl <- function(Q, r, K, sigma2, mpar, delta, catch, index)
    .Call("loglPellaTomC", c(Q,r,K,sigma2), mpar, delta, catch, index)
  initial <- structure(function(catch) return(list(Q=70, K=300, r=0.5, sigma2=1)),
    lower=rep(1e-6, 4), upper=rep(Inf, 4))
  return(list(model=index ~ .Call('PellatomC', catch, delta, mpar, c(Q, r, K)), 
    logl=logl, initial=initial))
} # }}}

# assess  {{{
setMethod("assess", signature(control="FLSP"),
   function(control, ...)
   mle(control)
) # }}}

# merge {{{
setMethod("merge", signature(x="FLStock", y="FLSP"),
  function(x, y, ...)
  {
    quant <- quant(stock(x))
    dnx <- dimnames(stock(x))
    dny <- dimnames(y@stock.n)

    # check dimensions match
    if(!all.equal(dnx[-2], dny[-2]))
      stop("Mismatch in dimensions: only year can differ between stock and assess")

    # same plusgroup
    if(x@range['plusgroup'] != x@range['plusgroup'])
      stop("Mismatch in plusgroup: x and y differ")

    # year ranges match?
    if(!all(dny[['year']] %in% dnx[['year']]))
    {
      years <- as.numeric(unique(c(dnx$year, dny$year)))
      x <- window(x, start=min(years), end=max(years))
      y <- window(y, start=min(years), end=max(years))
    }
    
    x <- transform(x, desc=paste(x@desc, "+ FLAssess:", y@name),
      stock.n=y@stock.n, harvest=y@harvest)
    x <- transform(x, range=c(unlist(dims(x)[c('min', 'max', 'plusgroup',
      'minyear', 'maxyear')]), x@range[c('minfbar', 'maxfbar')]))
        
    return(x)
  }
)   # }}}

# "+"      {{{
setMethod("+", signature(e1="FLStock", e2="FLAssess"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)
setMethod("+", signature(e1="FLAssess", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e2, e1))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)   # }}}

# biomass {{{
if (!isGeneric("biomass"))
	setGeneric("biomass", function(object, ...)
    	standardGeneric("biomass"))
setMethod('biomass', signature(object='FLSP'),
  function(object)
    return(fitted(object) / as.numeric(params(object)['Q',]) *
      as.numeric(params(object)['K',])))
# }}}

# Schaefer {{{
Schaefer <- function(catch, r, K, q)
{
  dm <- dimnames(catch)
  catch <- as.vector(catch)
  biomass <- rep(K, length(catch))
  for(y in seq(2, length(catch)))
    biomass[y] <- biomass[y-1] + r * biomass[y-1] * (1 - (1/K) * biomass[y-1] ) - catch[y]
  biomass[biomass <= 0] <- 1e-9
  print(q)
  return(FLQuant(q*biomass, dimnames=dm))
}


schaefer <- function()
{
  logl <- function(r, K, q, sigma2, catch, index)
  {
   sum(dnorm(log(index), window(log(Schaefer(catch, r, K, q)),
      start=dims(index)$minyear,end=dims(index)$maxyear), sqrt(sigma2), TRUE), na.rm=TRUE)
  }
  return(list(model=index~Schaefer(catch, r, K, q), logl=logl))
} # }}}

# Accesors  {{{
setMethod('catch', signature(object='FLSP'),
  function(object)
    return(object@catch)
)
setMethod('catch<-', signature(object='FLSP', value='FLQuant'),
  function(object, value)
  {
    object@catch <- value
    return(object)
  }
)

setMethod('index', signature(object='FLSP'),
  function(object)
    return(object@index)
)
setMethod('index<-', signature(object='FLSP', value='FLQuant'),
  function(object, value)
  {
    object@index <- value
    return(object)
  }
)
# }}}

# plot  {{{
setMethod('plot', signature(x='FLSP', y='missing'),
  function(x, y, ...)
  {
    # new trellis device
    trellis.device(new=FALSE)
    trellis.par.set(list(layout.heights = list(bottom.padding = -0.5,
      axis.xlab.padding = 0.5, xlab = -0.5), layout.widths = list(left.padding = -0.5,
      right.padding = -0.5, ylab.axis.padding = -0.5)))

    # upper plots data: biomass and residuals
    data <- FLQuants(biomass=biomass(x), residuals=residuals(x))

    print(xyplot(data~year|qname, data=data, scales=list(relation='free'),
      panel=function(x, y, ...)
      {
        if(panel.number() == 1)
          panel.xyplot(x, y, col='black', type='b', cex=0.8, pch=19)
        else if (panel.number() == 2)
        {
			    panel.xyplot(x, y, col='gray40', cex=0.8)
          panel.loess(x,y, col='red')
	    		panel.abline(a=0, b=0, lty=2, col='blue')
        }
		  }, xlab="", ylab=""), 
      split=c(1,1,1,2), more=TRUE)

    # panel function for lowe panel: index vs. fitted
    pfun <- function(x, y, groups, subscripts,...)
    {
      panel.grid(h=-1, v=2)
      panel.xyplot(x[groups=='index'], y[groups=='index'], type='b', pch=19, col='black')
      panel.xyplot(x[groups=='fitted'], y[groups=='fitted'], type='smooth', lwd=2, 
        col='red')
      panel.xyplot(x[groups=='fitted'], y[groups=='fitted'], type='b', lwd=1, lty=2,
        col='black', pch=1)
    }

    # data for lower panel
    data <- FLQuants(index=index(x), fitted=fitted(x))

    print(xyplot(index+fitted~year, data=model.frame(data), panel=pfun,
      scales=list(relation='free'), xlab="", ylab="",
      key=list(text=list(lab='index'),
          points=list(pch=19),
          text=list(lab='fitted'),
          points=list(pch=1))
       #   vp = viewport(x = unit(0.5, "npc"), y = unit(0.95, "npc")),
      ),
      split=c(1,2,1,2), more=FALSE)

    

  }
) # }}}

# harvest
