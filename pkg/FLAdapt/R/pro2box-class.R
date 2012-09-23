##########################################################################
### Pro2Box
### This file contains code just for the class pro2box, which is a control
### class for pro2box.exe 

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 
#########################################################################

validPro2box <- function(object){
	# All FLQuant objects must have same dimensions

	# If everything is fine
	return(TRUE)}

setClass("pro2box",
	representation("FLComp",
    options  ="numeric",             
		dir      ="character",
		files    ="character",
    mat      ="FLQuant",             
    grw      ="FLPar"),
	prototype=prototype(
    options  =c("model"=1,"nbox"=1,"niters"=NA,"ci"=80,"seed"=-911,"patch"=0,"yrbox"=1,"srbox"=0,"sex"=1,"wt"=1),    
		dir      ="/home/laurie/Desktop/flr/tests/FLAdapt/pro2box",
		files    =c("quotas.txt","naa.txt","faa.txt","waa.txt","caa.txt","maa.txt","sel.txt","recruit.txt","trans.txt","discards.txt")),
	validity=validPro2box)

setValidity("pro2box", validPro2box)
#remove(validPro2box)	# We do not need this function any more

is.pro2box = function(x)
	return(inherits(x, "pro2box"))


