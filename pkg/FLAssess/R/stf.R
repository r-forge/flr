# stf
# FLAssess/R/stf.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 13 Feb 2009 11:27
# $Id: stf.R,v 1.3 2008/06/11 10:32:24 ltkell Exp $

## stf :: Generic           {{{
if (!isGeneric("stf"))
    setGeneric("stf", function(object,...)
	    standardGeneric("stf"))

## stf in FLStock objects

setMethod('stf', signature(object='FLStock'),
  function(object, nyrs=3, wts.nyrs=3, fbar.nyrs=wts.nyrs, f.rescale=FALSE,
    arith.mean=TRUE, na.rm=TRUE)
  {
    dims <- dims(object)

    # window object
    object <- window(object, end=dims$maxyear + nyrs)

    # average slots


    # f.rescale

  }
)

# {{{
setMethod("stf", signature(object="FLStock"),
   function(object,nyrs=3,wts.nyrs=3,fbar.nyrs=3,f.rescale=FALSE,arith.mn=TRUE, na.rm=TRUE)
   {
   res       <-stf.old(object,nyrs=nyrs,fbar.nyrs=fbar.nyrs,wts.nyrs=wts.nyrs)
   proj.yrs  <-as.character((res@range["maxyear"]-nyrs+1):res@range["maxyear"])
   current.yr<-res@range["maxyear"]-nyrs
   if (f.rescale){
        fbar.age  <- as.character(object@range["minfbar"]:object@range["maxfbar"])
        fbar.nyrs <- as.character((res@range["maxyear"]-fbar.nyrs+1):res@range["maxyear"])
        mn.fbar <- apply(res@harvest[fbar.age,fbar.nyrs],3:6,mean)
        proj.fbar <- apply(res@harvest[fbar.age,proj.yrs  ],2:6,mean)
        ratio.fbar <- sweep(proj.fbar,3:6,mn.fbar,"/")

        res@harvest[fbar.age,proj.yrs,,,,] <-sweep(res@harvest[fbar.age,proj.yrs,,,,],2:6,ratio.fbar,"/")
        res@harvest[,proj.yrs[1:length(fmult)],,,,]<-sweep(res@harvest[,proj.yrs[1:length(fmult)],,,,],2,fmult,"*")
        }

    dmns <- dimnames(res@stock.n[1,proj.yrs])

    return(res)
    }
) # }}}

stf.old<-function(object,nyrs=3,wts.nyrs=3,fbar.nyrs=NA,disc.nyrs=NA,arith.mn=TRUE, na.rm=TRUE)
{
  # Works for 5D and 6D
  if (is.na(fbar.nyrs)) fbar.nyrs <- wts.nyrs
  if (is.na(disc.nyrs)) disc.nyrs <- wts.nyrs
  # Object checks
  if (!is(object, "FLStock"))
  stop("Object must be an 'FLStock' object")
  Dim <- dim(object@m)
  if (Dim[4]!=1) return("Not yet implemented for multiple seasons")
  if (Dim[5]!=1) return("Not yet implemented for multiple areas")
  if (Dim[3]>2) return("Not yet implemented for more than two units")

  # ----------- Set up new object -------------------

  fc.years <- as.character((object@range["maxyear"]+1):(object@range["maxyear"]+nyrs))
  weights.years <- as.character((object@range["maxyear"]-wts.nyrs+1):object@range["maxyear"])
  f.years <- as.character((object@range["maxyear"]-fbar.nyrs+1):object@range["maxyear"])
  disc.years <- as.character((object@range["maxyear"]-disc.nyrs+1):object@range["maxyear"])

  # The new object
  res <- window(object,start=object@range["minyear"],end=(object@range["maxyear"]+nyrs))
  res@desc    <- paste("Original data source: ", object@desc, ". Forecast for", nyrs, "years")

  # ------------ and fill in slots ---------------------

  # Weights, mat, m and swpn slots as mean of last wts.nyrs years
  weight.slots <- c("mat","m","harvest.spwn","m.spwn","discards.wt","catch.wt","landings.wt","stock.wt")
  for (slot.count in weight.slots)
    slot(res,slot.count) <- meanQuantYear(slot(res,slot.count),weights.years,fc.years,arith.mn,na.rm)

  # Harvest - just calc mean of last fbar.nyrs - no rescaling or anything funky
  res@harvest <- meanQuantYear(res@harvest,f.years,fc.years,arith.mn,na.rm)

  # Landings and discards - future proportions based on mean of previous proportions
  res@discards.n <- meanQuantYear(res@discards.n / (res@discards.n+res@landings.n), disc.years,fc.years,arith.mn,na.rm)
  res@landings.n[,fc.years] <- 1 - res@discards.n[,fc.years]

  return(res)
  }

# Calcs mean over the mean.yrs range and sticks the results in all years of the new.years range
meanQuantYear <- function(object,mean.years,new.years,arith.mn=FALSE,na.rm=TRUE)
{
  # Object is a 5 or 6D quant
  mean.years<-as.character(mean.years)
  new.years<-as.character(new.years)
  app.dims <- (1:length(dim(object)))[-2] # e.g 1,3,4,5,6
  # Make all new years values = 0 - clumsy method but whatever....
  object[,new.years]<-0.0
  if (arith.mn==FALSE)
    mean.bit <-  exp(apply(log(object[,mean.years]),app.dims,mean,na.rm=na.rm))
  else
    mean.bit <-  apply(object[,mean.years],app.dims,mean,na.rm=na.rm)
  object[,new.years] <- sweep(object[,new.years],app.dims,mean.bit,"+")
  return(object)
}
