# VPA - «Short one line description»

# Author: Laurie Kell, CEFAS
# Maintainer: Laurie Kell
# Additions:
# Last Change: 02 May 2008 11:46
# $Id: VPA.R 356 2009-10-16 07:17:21Z lauriekell $

# Reference:
# Notes:

validFLVPA <- function(object){

	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@harvest) == Dim))
		return("stock.n and harvest arrays must have same dimensions")

	# Everything is fine
	return(TRUE)}

setClass("FLVPA",
	representation(
               "FLComp",
		catch.n    ="FLQuant",
		stock.n    ="FLQuant",
		harvest    ="FLQuant"),
	prototype=prototype(
		catch.n    =new("FLQuant"),
		stock.n    =new("FLQuant"),
		harvest    =new("FLQuant")),
	validity=validFLVPA)

setValidity("FLVPA", validFLVPA)
remove(validFLVPA)

	setGeneric("VPA", function(stock, ...){
		value  <-  standardGeneric("VPA")
		value})


setMethod("VPA",signature(stock="FLStock"),
VPA.<-function(stock, fratio="missing", fit.plusgroup=TRUE, desc="",...) {
    
    Call <- match.call()
	
    if (!is(stock, "FLStock")) stop("stock must be an 'FLStock' object!")

    if (fit.plusgroup && is.na(stock@range["plusgroup"])) stop("can't fit a plusgroup if you specify no plusgroup in range of stock")

    # check only one unit, season and area
    if (dims(stock@catch.n)$unit > 1)
        stop("Currently only implemented for a single unit")
    if (dims(stock@catch.n)$season > 1)
        stop("Currently only implemented for a single season")
    if (dims(stock@catch.n)$area > 1)
        stop("Currently only implemented for a single area") 
    
    if ("minage"   %in% names(stock@range))
        minage <- stock@range["minage"]   else
    if ("min"      %in% names(stock@range))
        minage <- stock@range["min"]      else
    if ("minquant" %in% names(stock@range))
        minage <- stock@range["minquant"] else  stop("'minage' not found in range")

    if ("maxage"   %in% names(stock@range))
        maxage <- stock@range["maxage"]
    else
        if ("max"      %in% names(stock@range))
            maxage <- stock@range["max"]
        else
            if ("maxquant" %in% names(stock@range))
                maxage <- stock@range["maxquant"]
            else
                stop("'maxage' not found in range")

    stock@m <- stock@m[as.character(minage:maxage), , , ,]

    #### need to have n its in harvest & stock.n
    if (dims(m(      stock))$iter>1 | dims(stock.n(stock))$iter>1 |
        dims(harvest(stock))$iter>1 | dims(catch.n(stock))$iter>1){
        if (dims(stock.n(stock))$iter==1)
          stock.n(stock)<-propagate(stock.n(stock),dims(stock)$iter)
        if (dims(harvest(stock))$iter==1)
          harvest(stock)<-propagate(harvest(stock),dims(stock)$iter)
        }
        

    if (all(is.na(stock@catch.n)))
        stop("catch.n is not available")

    stock@stock.n[]<-NA
    
    ##fratio
    fratio.flag       <-rep(as.logical(FALSE),stock@range["maxyear"]-stock@range["minyear"]+1)
    fratio.           <-rep(as.numeric(NA),length(fratio.flag))
    names(fratio.flag)<-stock@range["minyear"]:stock@range["maxyear"]
    names(fratio.)    <-stock@range["minyear"]:stock@range["maxyear"]
    
    if (!missing(fratio) & !is.null(names(fratio)))
      {
      fratio.flag[names(fratio)] <- TRUE
      fratio.[names(fratio)]     <- fratio
      }
    else if (!missing(fratio) & is.null(names(fratio)))
      {
      fratio.flag[]              <- TRUE
      fratio.[]                  <- rep(fratio,length(fratio.flag))
      }

    if (fit.plusgroup==FALSE && any(fratio.flag))
        stop("Only valid to specify F Ratio if estimating plus group")
    
    res <-.Call("_FLVPA", stock, fit.plusgroup, fratio., fratio.flag)

    # Returns object of type of FLVPA
    # Finally, copy over the range and tidy up
    for (r in c("min", "max", "plusgroup", "minyear", "maxyear"))
        range(res)[[r]] <- range(stock)[[r]]
    units(res@harvest)<-"f"
    return(res)})


