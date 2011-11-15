### FLAdapt ####################################################################

### methods ####################################################################
### This file contains code just for the class FLAdapt

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 

# Test class
# Test if an object is of FLAdapt class
is.FLAdapt <- function(x)
	return(inherits(x, "FLAdapt"))
is.FLAdapt.control <- function(x)
	return(inherits(x, "FLAdapt.control"))

### End Methods #################################################################
