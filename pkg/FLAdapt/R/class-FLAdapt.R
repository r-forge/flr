##########################################################################
### FLAdapt
### This file contains code just for the class FLAdapt

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 
#########################################################################
## There are two main classes the Adapt class and the control class
## FLAdapt ##############################################################

validFLAdapt <- function(object){
	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@f) == Dim))
		return("n and f arrays must have same dimensions")
	# Everything is fine
	return(TRUE)}

setClass("FLAdapt",
	representation(
    "FLComp",
		call     ="character",
		control  ="FLAdaptControl",
		stock.n  ="FLQuant",
		harvest  ="FLQuant",
    diags    ="data.frame"),
	prototype=prototype(
		call     ="new(\"FLAdapt\")",
		control  =new("FLAdaptControl"),
		stock.n  =new("FLQuant"),
		harvest  =new("FLQuant")),
	validity=validFLAdapt)

setValidity("FLAdapt", validFLAdapt)
remove(validFLAdapt)	# We do not need this function any more
