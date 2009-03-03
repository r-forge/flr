# SepVPA - «Short one line description»
# FLAssess/R/SepVPA.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, Cefas
# $Id$

## FLSepVPA.control class {{{

validFLSepVPA.control <- function(object){
	if (!is.na(object@sep.nyr))
	if (object@sep.age < 0)
		return("seperable age must be > 0")
	if (object@sep.sel < 0)
		return("reference selectivity must be > 0")

	# Everything is fine
	return(TRUE)
}

setClass("FLSepVPA.control",
	representation(
    sep.nyr		   ="integer",   ## Number of years for separable model 
		sep.age		   ="integer",   ## Reference age for fitting the separable model  
		sep.sel      ="numeric"),  ## Are the age-structured indices are correlated across ages
	prototype=prototype(
		sep.nyr		   =as.integer(5),
		sep.age		   =as.integer(4),
		sep.sel	     =as.numeric(1.0)),
	validity=validFLSepVPA.control
)

setValidity("FLSepVPA.control", validFLSepVPA.control)
remove(validFLSepVPA.control)   # }}}

## FLSepVPA.control()   {{{

FLSepVPA.control <- function(sep.age="missing",sep.sel="missing",sep.nyr="missing"){

    res  <-new("FLSepVPA.control")
    
    if (missing(sep.age)) sep.age <- res@sep.age
    if (missing(sep.sel)) sep.sel <- res@sep.sel
    if (missing(sep.nyr)) sep.nyr <- res@sep.nyr
    
	res2 <- new("FLSepVPA.control",sep.nyr=as.integer(sep.nyr),
        sep.age=as.integer(sep.age),sep.sel=as.numeric(sep.sel))

	# Verify that this object is valid
  	test <- validObject(res2)

	if (!test)
        stop("Invalid object:", test)

	return(res2)
} # }}}

## SepVPA   {{{
if (!isGeneric("SepVPA"))
	setGeneric("SepVPA", function(stock, ...)
		standardGeneric("SepVPA"))

setMethod("SepVPA", signature(stock="FLStock"),
    function(stock, control=FLSepVPA.control(), ref.harvest="missing", fratio="missing",
        fit.plusgroup=TRUE, desc="", ...) {
        
        call <- match.call()
	
        # input checking
        if (fit.plusgroup && is.na(stock@range["plusgroup"]))
            stop("can't fit a plusgroup if you specify no plusgroup in range of stock")

        if (dims(stock@catch.n)$unit > 1)
            stop("Currently only implemented for a single unit")
        if (dims(stock@catch.n)$season > 1)
            stop("Currently only implemented for a single season")
        if (dims(stock@catch.n)$area > 1)
            stop("Currently only implemented for a single area") 
 
        # minage
        if ("minage"   %in% names(stock@range))
            minage <- stock@range["minage"]   
        else
            if ("min"      %in% names(stock@range))
                minage <- stock@range["min"]
            else
                if ("minquant" %in% names(stock@range))
                    minage <- stock@range["minquant"]
                else
                    stop("'minage' not found in range")
        # maxage
        if ("maxage" %in% names(stock@range))
            maxage <- stock@range["maxage"]
        else
            if ("max" %in% names(stock@range))
                maxage <- stock@range["max"]
            else
                if ("maxquant" %in% names(stock@range))
                    maxage <- stock@range["maxquant"]
                else  stop("'maxage' not found in range")
	                stock@m <- stock@m[as.character(minage:maxage),,,,]
        # catch.n
        if (all(is.na(stock@catch.n)))
            stop("catch.n is not available")

        stock@stock.n <- FLQuant(as.numeric(NA),dimnames=dimnames(stock@m))      

        if (missing(ref.harvest))
            ref.harvest = 1.0

        if (!is.na(control@sep.nyr)) {
            stock.sep <-trim(stock,year=(stock@range["maxyear"]-
                control@sep.nyr+1):stock@range["maxyear"])
            stock.vpa <-trim(stock,year= stock@range["minyear"]:(stock@range["maxyear"]-
                control@sep.nyr+1))
            res.sep    <-.Call("FLSepVPA", stock.sep, control, ref.harvest)
            if (missing(fratio))
                res.vpa <- VPA(stock.vpa, fit.plusgroup=fit.plusgroup)
            else
                res.vpa <- VPA(stock.vpa, fit.plusgroup=fit.plusgroup, fratio=fratio)
            res <- stock
    
            res@stock.n[dimnames(res.vpa@stock.n)$age,
                dimnames(res.vpa@stock.n)$year,,,] <- res.vpa@stock.n
            res@catch.n[dimnames(res.vpa@stock.n)$age,
                dimnames(res.vpa@stock.n)$year,,,] <- res.vpa@catch.n
            res@harvest[dimnames(res.vpa@stock.n)$age,
                dimnames(res.vpa@stock.n)$year,,,] <- res.vpa@harvest
            res@stock.n[dimnames(res.sep@stock.n)$age,
                dimnames(res.sep@stock.n)$year,,,] <- res.sep@stock.n
            res@catch.n[dimnames(res.sep@stock.n)$age,
                dimnames(res.sep@stock.n)$year,,,] <- res.sep@catch.n
            res@harvest[dimnames(res.sep@stock.n)$age,
                dimnames(res.sep@stock.n)$year,,,] <- res.sep@harvest
        } else
            res <-.Call("FLSepVPA", stock, control, ref.harvest)
        
        # output stock
        res2 <- new('FLAssess', stock.n=res@stock.n,
            catch.n=stock@catch.n,
            harvest=res@harvest,
		    index=new("FLQuants"),
    		index.res=new("FLQuants"),
	    	index.hat=new("FLQuants"),
		    index.var=new("FLQuants"))

    	if (!missing(desc))
            res2@desc <- as.character(desc)
    
    	res2@call <- as.character(call)
        units(res2@harvest)<-"f"

	return(res2)
    }
)   # }}}

# assess(FLSepVPA.control)    {{{
# assess(new('FLSepVPA.control'), FLStock())
setMethod("assess", signature(control="FLSepVPA.control"),
    function(control, ...){

    args <- c(list(...), list(control=control))

    # name stock object
    if(!'stock'%in%names(args))
        names(args)[match('FLStock', lapply(args, class))] <- 'stock'

    do.call('SepVPA', args)
   }
)   # }}}
