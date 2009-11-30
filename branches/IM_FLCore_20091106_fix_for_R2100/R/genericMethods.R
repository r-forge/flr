# genericMethods - «Short one line description»
# genericMethods

# Copyright 2009 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 25 Nov 2009 05:48:37 PM CET IM:

# name, name<-
if(!isGeneric('name'))
  setGeneric('name', function(object, ...) standardGeneric('name'))
if(!isGeneric('name<-'))
  setGeneric('name<-', function(object, ..., value) standardGeneric('name<-'))

# desc
if(!isGeneric('desc'))
  setGeneric('desc', function(object, ...) standardGeneric('desc'))
if(!isGeneric('desc<-'))
  setGeneric('desc<-', function(object, ..., value) standardGeneric('desc<-'))

# FLStock
setGeneric('FLStock', function(object, ...)
		standardGeneric('FLStock'))

# catch
if(!isGeneric('catch'))
  setGeneric('catch', function(object, ...) standardGeneric('catch'))
if(!isGeneric('catch<-'))
  setGeneric('catch<-', function(object, ..., value) standardGeneric('catch<-'))

# catch.n
if(!isGeneric('catch.n'))
  setGeneric('catch.n', function(object, ...) standardGeneric('catch.n'))
if(!isGeneric('catch.n<-'))
  setGeneric('catch.n<-', function(object, ..., value) standardGeneric('catch.n<-'))

# catch.wt
if(!isGeneric('catch.wt'))
  setGeneric('catch.wt', function(object, ...) standardGeneric('catch.wt'))
if(!isGeneric('catch.wt<-'))
  setGeneric('catch.wt<-', function(object, ..., value) standardGeneric('catch.wt<-'))

# discards
if(!isGeneric('discards'))
  setGeneric('discards', function(object, ...) standardGeneric('discards'))
if(!isGeneric('discards<-'))
  setGeneric('discards<-', function(object, ..., value) standardGeneric('discards<-'))

# discards.n
if(!isGeneric('discards.n'))
  setGeneric('discards.n', function(object, ...) standardGeneric('discards.n'))
if(!isGeneric('discards.n<-'))
  setGeneric('discards.n<-', function(object, ..., value) standardGeneric('discards.n<-'))

# discards.wt
if(!isGeneric('discards.wt'))
  setGeneric('discards.wt', function(object, ...) standardGeneric('discards.wt'))
if(!isGeneric('discards.wt<-'))
  setGeneric('discards.wt<-', function(object, ..., value) standardGeneric('discards.wt<-'))

# landings
if(!isGeneric('landings'))
  setGeneric('landings', function(object, ...) standardGeneric('landings'))
if(!isGeneric('landings<-'))
  setGeneric('landings<-', function(object, ..., value) standardGeneric('landings<-'))

# landings.n
if(!isGeneric('landings.n'))
  setGeneric('landings.n', function(object, ...) standardGeneric('landings.n'))
if(!isGeneric('landings.n<-'))
  setGeneric('landings.n<-', function(object, ..., value) standardGeneric('landings.n<-'))

# landings.wt
if(!isGeneric('landings.wt'))
  setGeneric('landings.wt', function(object, ...) standardGeneric('landings.wt'))
if(!isGeneric('landings.wt<-'))
  setGeneric('landings.wt<-', function(object, ..., value) standardGeneric('landings.wt<-'))

# stock
if(!isGeneric('stock'))
  setGeneric('stock', function(object, ...) standardGeneric('stock'))
if(!isGeneric('stock<-'))
  setGeneric('stock<-', function(object, ..., value) standardGeneric('stock<-'))

# stock.n
if(!isGeneric('stock.n'))
  setGeneric('stock.n', function(object, ...) standardGeneric('stock.n'))
if(!isGeneric('stock.n<-'))
  setGeneric('stock.n<-', function(object, ..., value) standardGeneric('stock.n<-'))

# stock.wt
if(!isGeneric('stock.wt'))
  setGeneric('stock.wt', function(object, ...) standardGeneric('stock.wt'))
if(!isGeneric('stock.wt<-'))
  setGeneric('stock.wt<-', function(object, ..., value) standardGeneric('stock.wt<-'))

# m
if(!isGeneric('m'))
  setGeneric('m', function(object, ...) standardGeneric('m'))
if(!isGeneric('m<-'))
  setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# m.spwn
if(!isGeneric('m.spwn'))
  setGeneric('m.spwn', function(object, ...) standardGeneric('m.spwn'))
if(!isGeneric('m.spwn<-'))
  setGeneric('m.spwn<-', function(object, ..., value) standardGeneric('m.spwn<-'))

# harvest
if(!isGeneric('harvest'))
  setGeneric('harvest', function(object, catch, ...) standardGeneric('harvest'))
if(!isGeneric('harvest<-'))
  setGeneric('harvest<-', function(object, ..., value) standardGeneric('harvest<-'))

# harvest.spwn
if(!isGeneric('harvest.spwn'))
  setGeneric('harvest.spwn', function(object, ...) standardGeneric('harvest.spwn'))
if(!isGeneric('harvest.spwn<-'))
  setGeneric('harvest.spwn<-', function(object, ..., value) standardGeneric('harvest.spwn<-'))

# mat
if(!isGeneric('mat'))
  setGeneric('mat', function(object, ...) standardGeneric('mat'))
if(!isGeneric('mat<-'))
  setGeneric('mat<-', function(object, ..., value) standardGeneric('mat<-'))

# computeLandings
if (!isGeneric("computeLandings"))
setGeneric("computeLandings", function(object, ...)
		standardGeneric("computeLandings"))

# computeDiscards
if (!isGeneric("computeDiscards"))
	setGeneric("computeDiscards", function(object, ...)
		standardGeneric("computeDiscards"))

# computeCatch
if (!isGeneric("computeCatch"))
	setGeneric("computeCatch", function(object, ...)
		standardGeneric("computeCatch"))

# computeStock
if (!isGeneric("computeStock"))
setGeneric("computeStock", function(object, ...)
		standardGeneric("computeStock"))

# ssb
if (!isGeneric("ssb"))
	setGeneric("ssb", function(object, ...)
		standardGeneric("ssb"))

# fbar
if (!isGeneric("fbar"))
	setGeneric("fbar", function(object, ...)
		standardGeneric("fbar"))

# as.FLStock
if (!isGeneric("as.FLStock"))
	setGeneric("as.FLStock", function(object, ...)
		standardGeneric("as.FLStock"))

# ssbpurec
if (!isGeneric("ssbpurec"))
	setGeneric("ssbpurec", function(object, ...)
		standardGeneric("ssbpurec"))

# n
if(!isGeneric('n'))
  setGeneric('n', function(object, ...) standardGeneric('n'))
if(!isGeneric('n<-'))
  setGeneric('n<-', function(object, ..., value) standardGeneric('n<-'))

# m
if(!isGeneric('m'))
  setGeneric('m', function(object, ...) standardGeneric('m'))
if(!isGeneric('m<-'))
  setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# wt
if(!isGeneric('wt'))
  setGeneric('wt', function(object, ...) standardGeneric('wt'))
if(!isGeneric('wt<-'))
  setGeneric('wt<-', function(object, ..., value) standardGeneric('wt<-'))

# fec
if(!isGeneric('fec'))
  setGeneric('fec', function(object, ...) standardGeneric('fec'))
if(!isGeneric('fec<-'))
  setGeneric('fec<-', function(object, ..., value) standardGeneric('fec<-'))

# spwn
if(!isGeneric('spwn'))
  setGeneric('spwn', function(object, ...) standardGeneric('spwn'))
if(!isGeneric('spwn<-'))
  setGeneric('spwn<-', function(object, ..., value) standardGeneric('spwn<-'))

# FLBiol
setGeneric('FLBiol', function(object, ...)
  standardGeneric('FLBiol'))

# mean.lifespan
setGeneric("mean.lifespan", function(x, ...)
	standardGeneric("mean.lifespan"))

# as.FLBiol
setGeneric("as.FLBiol", function(object, ...)
  standardGeneric("as.FLBiol"))

# ssn
setGeneric("ssn", function(object, ...)
  standardGeneric("ssn"))

# leslie
setGeneric("leslie", function(object, ...)
	standardGeneric("leslie"))

# r
setGeneric("r", function(object, ...)
  standardGeneric("r"))

# survprob
setGeneric("survprob", function(object, ...)
  standardGeneric("survprob"))

# rec
setGeneric("rec", function(object, ...)
  standardGeneric("rec"))

# FLMetier
setGeneric('FLMetier', function(catches, ...)
		standardGeneric('FLMetier'))

# gear
if(!isGeneric('gear'))
  setGeneric('gear', function(object, ...) standardGeneric('gear'))
if(!isGeneric('gear<-'))
  setGeneric('gear<-', function(object, ..., value) standardGeneric('gear<-'))

# effshare
if(!isGeneric('effshare'))
  setGeneric('effshare', function(object, ...) standardGeneric('effshare'))
if(!isGeneric('effshare<-'))
  setGeneric('effshare<-', function(object, ..., value) standardGeneric('effshare<-'))

# vcost
if(!isGeneric('vcost'))
  setGeneric('vcost', function(object, ...) standardGeneric('vcost'))
if(!isGeneric('vcost<-'))
  setGeneric('vcost<-', function(object, ..., value) standardGeneric('vcost<-'))

# catches
if(!isGeneric('catches'))
  setGeneric('catches', function(object, ...) standardGeneric('catches'))
if(!isGeneric('catches<-'))
  setGeneric('catches<-', function(object, ..., value) standardGeneric('catches<-'))

# effort
# fcost
if(!isGeneric('fcost'))
  setGeneric('fcost', function(object, ...) standardGeneric('fcost'))
if(!isGeneric('fcost<-'))
  setGeneric('fcost<-', function(object, ..., value) standardGeneric('fcost<-'))

# capacity
if(!isGeneric('capacity'))
  setGeneric('capacity', function(object, ...) standardGeneric('capacity'))
if(!isGeneric('capacity<-'))
  setGeneric('capacity<-', function(object, ..., value) standardGeneric('capacity<-'))

# crewshare
if(!isGeneric('crewshare'))
  setGeneric('crewshare', function(object, ...) standardGeneric('crewshare'))
if(!isGeneric('crewshare<-'))
  setGeneric('crewshare<-', function(object, ..., value) standardGeneric('crewshare<-'))

# metiers
if(!isGeneric('metiers'))
  setGeneric('metiers', function(object, ...) standardGeneric('metiers'))
if(!isGeneric('metiers<-'))
  setGeneric('metiers<-', function(object, ..., value) standardGeneric('metiers<-'))

# FLFleet
setGeneric('FLFleet', function(object, ...)
		standardGeneric('FLFleet'))

# metier
setGeneric('metier', function(object, metier, ...)
		standardGeneric('metier'))
setGeneric('metier<-', function(object, metier, ..., value)
		standardGeneric('metier<-'))

# type
setGeneric('type', function(object, ...)
		standardGeneric('type'))
setGeneric('type<-', function(object, ..., value)
		standardGeneric('type<-'))

# distribution 
setGeneric('distribution', function(object, ...)
		standardGeneric('distribution'))
setGeneric('distribution<-', function(object, ..., value)
		standardGeneric('distribution<-'))

# index
setGeneric('index', function(object, ...)
		standardGeneric('index'))
setGeneric('index<-', function(object, ..., value)
		standardGeneric('index<-'))

# index.var
setGeneric('index.var', function(object, ...)
		standardGeneric('index.var'))
setGeneric('index.var<-', function(object, ..., value)
		standardGeneric('index.var<-'))

# catch.n
setGeneric('catch.n', function(object, ...)
		standardGeneric('catch.n'))
setGeneric('catch.n<-', function(object, ..., value)
		standardGeneric('catch.n<-'))

# catch.wt
setGeneric('catch.wt', function(object, ...)
		standardGeneric('catch.wt'))
setGeneric('catch.wt<-', function(object, ..., value)
		standardGeneric('catch.wt<-'))

# effort
setGeneric('effort', function(object, ...)
		standardGeneric('effort'))
setGeneric('effort<-', function(object, ..., value)
		standardGeneric('effort<-'))

# sel.pattern
setGeneric('sel.pattern', function(object, ...)
		standardGeneric('sel.pattern'))
setGeneric('sel.pattern<-', function(object, ..., value)
		standardGeneric('sel.pattern<-'))

# index.q
setGeneric('index.q', function(object, ...)
		standardGeneric('index.q'))
setGeneric('index.q<-', function(object, ..., value)
		standardGeneric('index.q<-'))

# FLIndex
setGeneric('FLIndex', function(object, ...)
	standardGeneric('FLIndex'))

# as.FLindex
setGeneric("as.FLIndex", function(object, ...)
  standardGeneric("as.FLIndex"))

# model
setGeneric('model', function(object, ...)
		standardGeneric('model'))
setGeneric('model<-', function(object, ..., value)
		standardGeneric('model<-'))

# logl
setGeneric('logl', function(object, ...)
		standardGeneric('logl'))
setGeneric('logl<-', function(object, ..., value)
		standardGeneric('logl<-'))

# grad
setGeneric('grad', function(object, ...)
		standardGeneric('grad'))
setGeneric('grad<-', function(object, ..., value)
		standardGeneric('grad<-'))

# initial
setGeneric('initial', function(object, ...)
		standardGeneric('initial'))
setGeneric('initial<-', function(object, ..., value)
		standardGeneric('initial<-'))

# params
setGeneric('params', function(object, ...)
		standardGeneric('params'))
setGeneric('params<-', function(object, ..., value)
		standardGeneric('params<-'))

# logLik
setGeneric('logLik', function(object, ...)
		standardGeneric('logLik'))
setGeneric('logLik<-', function(object, ..., value)
		standardGeneric('logLik<-'))

# vcov
setGeneric('vcov', function(object, ...)
		standardGeneric('vcov'))
setGeneric('vcov<-', function(object, ..., value)
		standardGeneric('vcov<-'))

# details
setGeneric('details', function(object, ...)
		standardGeneric('details'))
setGeneric('details<-', function(object, ..., value)
		standardGeneric('details<-'))

# residuals
setGeneric('residuals', function(object, ...)
		standardGeneric('residuals'))
setGeneric('residuals<-', function(object, ..., value)
		standardGeneric('residuals<-'))

# fitted
setGeneric('fitted', function(object, ...)
		standardGeneric('fitted'))
setGeneric('fitted<-', function(object, ..., value)
		standardGeneric('fitted<-'))

# rec
setGeneric('rec', function(object, ...)
		standardGeneric('rec'))
setGeneric('rec<-', function(object, ..., value)
		standardGeneric('rec<-'))

# ssb
setGeneric('ssb', function(object, ...)
		standardGeneric('ssb'))
setGeneric('ssb<-', function(object, ..., value)
		standardGeneric('ssb<-'))

# covar
setGeneric('covar', function(object, ...)
		standardGeneric('covar'))
setGeneric('covar<-', function(object, ..., value)
		standardGeneric('covar<-'))

