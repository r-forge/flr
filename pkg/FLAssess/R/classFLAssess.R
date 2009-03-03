# FLAssess-class - «Short one line description»
# FLAssess-class

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 01 Sep 2008 16:02
# $Id$

# Reference:
# Notes:

# TODO Thu 10 Jul 2008 09:28:03 AM CEST IM:


## FLAssess		{{{
validFLAssess <- function(object){

	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@harvest) == Dim))
		return("stock.n and harvest arrays must have same dimensions")

	# Everything is fine
	return(TRUE)
}
setClass("FLAssess",
	representation(
    "FLComp",
		catch.n    ="FLQuant",
		stock.n    ="FLQuant",
		harvest    ="FLQuant",
		index.name ="character",
		index.range="list",
		index      ="FLQuants",
		index.res  ="FLQuants",
		index.hat  ="FLQuants",
		index.var  ="FLQuants",
		call       ="character"),
	prototype=prototype(
		catch.n    =new("FLQuant"),
		stock.n    =new("FLQuant"),
		harvest    =new("FLQuant"),
		index.name =character(0),
		index.range=new('list'),
		index      =new('FLQuants'),
		index.res  =new('FLQuants'),
    index.hat  =new('FLQuants'),
    index.var  =new('FLQuants'),
   	call       ="new(\"FLAssess\")"),
	validity=validFLAssess
)
setValidity("FLAssess", validFLAssess)
remove(validFLAssess)
#invisible(createFLAccesors(new("FLAssess")))	# }}}


## FLAssess.retro	{{{
validFLAssess.retro <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)
	# Make sure the list contains only numeric items
	for (i in 1:length(object))
		if (!inherits(object[[i]], "numeric"))
			return("Items must be numeric objects!")
	# Everything is fine
	return(TRUE)
}

setClass("FLAssess.retro",
	representation(
		desc    ="character",
    ssb     ="FLQuants",
		recruits="FLQuants",
		harvest ="FLQuants"),
	prototype=prototype(
		desc    =character(0),
		ssb     =new('FLQuants'),
		recruits=new('FLQuants'),
		harvest =new('FLQuants')
    ),
	validity=validFLAssess.retro
)

setValidity("FLAssess.retro", validFLAssess.retro)
remove(validFLAssess.retro)	# }}}
## FLAssess.fval	{{{
validFLAssess.fval <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)
	# Make sure the list contains only numeric items
	for (i in 1:length(object))
		if (!inherits(object[[i]], "numeric"))
			return("Items must be numeric objects!")
	# Everything is fine
	return(TRUE)
}

setClass("FLAssess.fval",
	representation(
		desc    ="character",
 	  ssb     ="FLQuants",
    catch   ="FLQuants",
		landings="FLQuants",
		discards="FLQuants",
		recruits="FLQuants",
		harvest ="FLQuants"),
	prototype=prototype(
		desc    =character(0),
		ssb     =new('FLQuants'),
		catch   =new('FLQuants'),
		landings=new('FLQuants'),
		discards=new('FLQuants'),
		recruits=new('FLQuants'),
		harvest =new('FLQuants')
    ),
	validity=validFLAssess.fval
)
