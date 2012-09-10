# FLAssess-class - «Short one line description»
# FLAssess-class

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas & Robert Scott, JRC
# $Id$

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
