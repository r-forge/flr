library(FLCore)
# FLFitVPA - Class and methods for Surplus Production models
# FLAssess/R/FLFitVPA.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id:  $

# TODO 13Dec07 predict: index or biomass?

# class FLFitVPA {{{
setClass('FLFitVPA', representation(
  'FLModel',
  catch='FLQuant',
  index='FLQuant',
  biomass='FLQuant',
  mpar='numeric',
  delta='numeric')
) # }}}

# FLFitVPA()	{{{
setGeneric('FLFitVPA', function(model, ...)
		standardGeneric('FLFitVPA'))
setMethod('FLFitVPA', signature(model='ANY'),
  function(model, ...)
    return(FLModel(model, ..., class='FLFitVPA')))
setMethod('FLFitVPA', signature(model='missing'),
	function(...)
		return(FLModel(formula(NULL), ..., class='FLFitVPA'))) # }}}

# pellatom {{{
pellatom <- function(catch, r, K, Q, mpar=1.0, delta=1)
  {
    dm <- dimnames(catch)
    catch <- as.vector(catch)
    biomass <- rep(delta, length(catch))
    for(y in seq(2, length(catch)))
      biomass[y] <- biomass[y-1] + r * biomass[y-1] * (1 - biomass[y-1] ^ (mpar)) - catch[y]/K
    biomass[biomass <= 0] <- 1e-9
    return(FLQuant(Q*biomass, dimnames=dm))
  }

logl <- function(Q, r, K, sigma2, mpar, delta, catch, index)
   {
    sum(dnorm(log(index), window(log(pellatom(catch, r, K, Q, mpar, delta)),
      start=dims(index)$minyear,end=dims(index)$maxyear), sqrt(sigma2), TRUE), na.rm=TRUE)
   }
 # }}}

# Fmsy  {{{
if (!isGeneric("Fmsy"))
  setGeneric("Fmsy", function(obj, ...)
	  standardGeneric("Fmsy"))

setMethod('Fmsy', signature(obj='FLFitVPA'),
function(obj) {
   r<-params(obj)[,"r"]
   m<-obj@mpar

   r/(1+m)
   })
# }}}

# Bmsy  {{{
if (!isGeneric("Bmsy"))
  setGeneric("Bmsy", function(obj, ...)
	  standardGeneric("Bmsy"))

setMethod('Bmsy', signature(obj='FLFitVPA'),
function(obj) {
   K<-params(obj)[,"K"]
   m<-obj@mpar

   K*(1/(1+m))^m
   })
# }}}

# msy  {{{
if (!isGeneric("msy"))
  setGeneric("msy", function(obj, ...)
	  standardGeneric("msy"))

setMethod('msy', signature(obj='FLFitVPA'),
function(obj) {
   r<-params(obj)[,"r"]
   K<-params(obj)[,"K"]
   m<-obj@mpar

   r*K*(1/(1+m))^(1/m+1)
   })
# }}}

# biomass  {{{
if (!isGeneric("biomass"))
  setGeneric("biomass", function(obj, ...)
	  standardGeneric("biomass"))

setMethod('biomass', signature(obj='FLFitVPA'),
function(obj) {
  fitted(obj)/c(params(obj)[,"Q"])*c(params(obj)[,"K"])
  })
  
#load("C:\\FLR\\Examples\\FLFitVPA\\alb.RData")
#load('alb.RData')

alb@mpar<-1
alb        <-mle(alb, start=list(Q=70, r=0.3, K=300, sigma2=1), control=list(REPORT=1, trace=1),lower=c(0.001, 0.001, 0.001, 0.001), upper=c(Inf, Inf, Inf, Inf))

# plot
xyplot(data~year|qname,         FLQuants(catch=alb@catch,    index=alb@index), type='b')
xyplot(data~year, groups=qname, FLQuants(fitted=fitted(alb), index=alb@index), type='b')

r<-c(alb@params[1,"r"])
K<-c(alb@params[1,"K"])
m<-alb@mpar

catch.hat  <- r * seq(0,K,K/100) - r*(seq(0,K,K/100)^m)/K
biomass    <-seq(0,1,0.01)*c(params(alb)[1,"K"])
plot(seq(0,K,K/100),catch.hat,type="l",ylim=c(0,50),ylab="Catch",xlab="Biomass")
points(alb@biomass,alb@catch,type="b")
abline(v=Bmsy(alb))
abline(h=msy(alb))


plot(catch.hat~biomass,type="l", ylim=c(0,50), xlim=c(0,350))
points(c(alb@biomass),c(alb@catch),type="b")

abline(v=Bmsy(alb))
abline(h=msy(alb))
