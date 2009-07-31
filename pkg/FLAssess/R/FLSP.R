# FLSP - Class and methods for Surplus Production models
# FLAssess/R/FLSP.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

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

# ************** Accessors **************************************
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

# ***********	PellaTom with estimated Q ************
# PellaTom {{{
PellaTom <- function(catch, r, K, Q, mpar, delta)
{
  dm <- dimnames(catch)
  catch <- as.vector(catch)
  biomass <- rep(delta, length(catch))
  for(y in seq(2, length(catch)))
    biomass[y] <- biomass[y-1] + (r / (mpar-1)) * biomass[y-1] * (1 - (biomass[y-1] / K) ^ (mpar-1)) - catch[y-1]
  biomass[biomass <= 0] <- 1e-9
  #return(list(indexhat=FLQuant(Q*biomass, dimnames=dm), biomass=FLQuant(biomass,dimnames=dm)))
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

# ************ PellaTom with calculated  Q***************

PellaTom_calcQ <- function(catch, index, r, K, mpar, delta)
{
    #browser()
  dm <- dimnames(catch)
  catch <- as.vector(catch)
  index <- as.vector(index)
  biomass <- rep(delta, length(catch))
  for(y in seq(2, length(catch)))
    biomass[y] <- biomass[y-1] + (r / (mpar-1)) * biomass[y-1] * (1 - (biomass[y-1] / K) ^ (mpar-1)) - catch[y-1]
  biomass[biomass <= 0] <- 1e-9
  
  mnbio <- (biomass[-length(biomass)] + biomass[-1]) / 2
  q <- sum(mnbio*index[-length(biomass)])/sum(mnbio*mnbio)
  # Basically saying that index in year y measures mean of biomass y and y+1
  indexhat <- FLQuant(q*mnbio,dimnames=dm)
  # Need to set final year to NA as we have effectively lost last year of index
  indexhat[,dm$year[length(dm$year)]] <- NA
  return(indexhat)
}

#PellaTom_noQ(alb.sp.nq@catch, alb.sp.nq@index, 0.5,200,1,2,250)
# Need to pass in index to the PellaTom function
pellatom_calcQ <- function()
{
  logl <- function(r, K, sigma2, mpar, delta, catch, index)
  {
  # Need to lose final year of index due to q calc 
    sum(dnorm(log(index[,-dim(index)[2]]), window(log(PellaTom_calcQ(catch, index, r, K, mpar, delta)),
      start=dims(index)$minyear,end=dims(index)$maxyear-1), sqrt(sigma2), TRUE), na.rm=TRUE)
  }
  initial <- structure(function(catch) return(TRUE),
    lower=rep(1e-6, 4), upper=rep(Inf, 4))
  return(list(model=index~PellaTom_calcQ(catch, index, r, K, mpar, delta), logl=logl,
    initial=initial))
} # }}}

#***************** Methods **********************************

# biomass {{{
if (!isGeneric("biomass"))
	setGeneric("biomass", function(object, ...)
    	standardGeneric("biomass"))

    # Just calculates and returns the biomass
setMethod('biomass', signature(object='FLSP'),
  function(object) {
      #browser()
    dm <- dimnames(object@catch)
    biomass <- FLQuant(object@delta,dimnames=dm)
    # Mixing Quants and Pars is fairly ugly
    for(y in 2:length(dm$year))
	# Pretty ugly with either as.numeric or sweeps
	#biomass[,y] <- biomass[,y-1] + as.numeric(object@params["r",] / (object@mpar-1)) * biomass[,y-1] * (1 - (biomass[,y-1] / as.numeric(object@params["K",])) ^ (object@mpar-1)) - object@catch[,y-1]
	biomass[,y]<-biomass[,y-1] + sweep(biomass[,y-1],6,object@params["r",]/(object@mpar-1),"*") * (1 - sweep(biomass[,y-1], 6, object@params["K",],"/")^(object@mpar-1)) - object@catch[,y-1]
    biomass[biomass <= 0] <- 1e-9
    return(biomass)})
    # }}}

# harvest rate {{{
if (!isGeneric("harvest.rate"))
	setGeneric("harvest.rate", function(object, ...)
    	standardGeneric("harvest.rate"))
setMethod('harvest.rate', signature(object='FLSP'),
  function(object)
    return(catch(object) / biomass(object)))
# }}}

# plot {{{
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

# assess  {{{
setMethod("assess", signature(control="FLSP"),
   function(control, ...)
   fmle(control)
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
#
## "+"      {{{
##setMethod("+", signature(e1="FLStock", e2="FLAssess"),
##	function(e1, e2) {
##    if(validObject(e1) & validObject(e2))
##      return(merge(e1, e2))
##    else
##      stop("Input objects are not valid: validObject == FALSE")
##    }
##)
##setMethod("+", signature(e1="FLAssess", e2="FLStock"),
##	function(e1, e2) {
##    if(validObject(e1) & validObject(e2))
##      return(merge(e2, e1))
##    else
##      stop("Input objects are not valid: validObject == FALSE")
##    }
##)   # }}}

# }}}

# Schaefer {{{
#Schaefer <- function(catch, r, K, q)
#{
#  dm <- dimnames(catch)
#  catch <- as.vector(catch)
#  biomass <- rep(K, length(catch))
#  for(y in seq(2, length(catch)))
#    biomass[y] <- biomass[y-1] + r * biomass[y-1] * (1 - (1/K) * biomass[y-1] ) - catch[y]
#  biomass[biomass <= 0] <- 1e-9
#  print(q)
#  return(FLQuant(q*biomass, dimnames=dm))
#}
#
#
#schaefer <- function()
#{
#  logl <- function(r, K, q, sigma2, catch, index)
#  {
#   sum(dnorm(log(index), window(log(Schaefer(catch, r, K, q)),
#      start=dims(index)$minyear,end=dims(index)$maxyear), sqrt(sigma2), TRUE), na.rm=TRUE)
#  }
#  return(list(model=index~Schaefer(catch, r, K, q), logl=logl))
#} # }}}
#
