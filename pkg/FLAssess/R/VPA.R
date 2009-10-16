# VPA - «Short one line description»

# Author: Laurie Kell, CEFAS
# Maintainer: Laurie Kell
# Additions:
# Last Change: 02 May 2008 11:46
# $Id$

# Reference:
# Notes:

setClass('FLVPA', representation('FLAssess'))

## VPA  {{{
if (!isGeneric("VPA")) {
	setGeneric("VPA", function(stock, ...){
		value  <-  standardGeneric("VPA")
		value
	})
}
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
    return(res)


# Following not needed
#    res2<-new("FLAssess")
#    
#    if (class(res) != "FLAssess") 
#        return(res)
#  	if (!missing(desc))
#        res2@desc <- as.character(desc)
#
#	  res2@call <- as.character(Call)
#
#
#    if (length(dims(res2@stock.n))==6)
#       res2@stock.n <- FLQuant(res@stock.n@.Data,fill.iter=TRUE)
#    else
#       res2@stock.n <- res@stock.n
#
#    if (length(dims(res2@catch.n))==6)
#       res2@catch.n <- FLQuant(res@catch.n@.Data,fill.iter=TRUE)
#    else
#       res2@catch.n <- res@catch.n
#
#    if (length(dims(res2@harvest))==6)
#       res2@harvest <- FLQuant(res@harvest@.Data,fill.iter=TRUE)
#    else
#       res2@harvest <- res@harvest
#
#    units(res2@harvest)<-"f"
#
#    # Finally, copy over the range
#    for (r in c("min", "max", "plusgroup", "minyear", "maxyear"))
#        range(res2)[[c]] <- range(stock)[[c]]
#
#	  return(res2)
    }
)   # }}}
