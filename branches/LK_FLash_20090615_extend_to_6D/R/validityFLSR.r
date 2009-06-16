# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 13 Mar 2009 16:36
# $Id: fwd.R 232 2009-04-30 15:44:58Z fscott $


setGeneric('validSRPar', function(object, ...)
		standardGeneric('validSRPar'))


.validSRPar<-function(object, sr, yrs=NULL, availability=NULL)
     {
     #### check that sr has dims in FLQuant
     if (!all(names(sr) %in% c("params","year","unit","season","area","iter")))
        stop("dims in sr not recognised")

     #### Check yrs
     if (!is.null(yrs)){
        yrs<-ac(yrs)
        if (!all(yrs %in% ac(dims(object)$minyear:dims(object)$maxyear)))
           stop("yrs exceed years in object")
     }else
        yrs<-ac(dims(object)$minyear:dims(object)$maxyear)

     if ("year" %in% names(sr)){
        if (!all(yrs %in% dimnames(sr)$year))
           stop("yrs exceed years in sr")}
     else{
         dmns  <-dimnames(sr)

         params<-list(params=dmns$params,year=yrs,dmns[[-1]])
         names(params)[-(1:2)]<-names(dmns[-1])
         #### CHECK RECYCLING
         sr<-FLPar(c(sr),dimnames=params)
         }

     #### create FLQuant compatible FLPar
     sr <-FLPar(as.FLQuant(as.data.frame(sr)))

     #### Check availability
     #### Needed to distribute recruits if SR$area==1
     if (dims(object)$area>1 & dims(as.FLQuant(sr))$area==1){
        if (is.null(availability))
           stop("availability needs to be provided if multiple areas not in SR")

        if (!all(yrs %in% ac(dims(availability)$minyear:dims(availability)$maxyear)))
           stop("years in availability mismatch")
       }

     #### Check iters
     niter<-unique(c(dims(object)$iter,1))

     if (!(dims(sr)$iter %in% niter))
        stop("Iters in sr don´t match those in object")

     if (!is.null(availability))
        if (!(dims(availability)$iter %in% niter))
           stop("Iters in availability don´t match those in object")

     dmns<-list(params=dimnames(sr)$params,
                year  =yrs,
                unit  =dimnames(m(object))$unit,
                season=dimnames(m(object))$season,
                area  =dimnames(m(object))$area,
                iter  =dimnames(sr)$iter)

     #### check units, seasons and areas
     if (any(unlist(dims(        object)[c("season","area","unit")])>1) |
         any(unlist(dims(as.FLQuant(sr))[c("season","area","unit")])>1)){

        #### check units
        if (!all(dimnames(sr)$unit %in% dimnames(m(object))$unit))
           stop("unit in sr and object don´t match")

        #### check season
        if (!all(dimnames(sr)$season %in% dimnames(m(object))$season))
           stop("season in sr and object don´t match")

        #### check area
        if (!all(dimnames(sr)$area %in% dimnames(m(object))$area))
           stop("area in sr and object don´t match")
        }

     res<-FLQuant(as.numeric(NA),dimnames=dmns)

     res[,dimnames(sr)$year,dimnames(sr)$unit,dimnames(sr)$season,dimnames(sr)$area,]<-as.FLQuant(sr)
     
     return(FLPar(res))
     }

setMethod('validSRPar', signature(object='FLStock'),
  function(object, sr, yrs=NULL, availability=NULL)
     {
     return(.validSRPar(object,sr,yrs,availability))
     })

setMethod('validSRPar', signature(object='FLBiol'),
  function(object, sr, yrs, availability)
     {
     return(.validSRPar(object,sr,yrs,availability))
     })

#setMethod('validSRPar', signature(object='FLBRP'),
#  function(object, sr, yrs)
#     {
#     return(.validSRPar(object,sr,yrs,availability(object)))
#     })

validSRRes<-function(object,sr,res){
   if (dims(res)$iter   != dims(object)$iter) stop("iters in residuals and object don´t match")
   if (dims(res)$year   != dims(sr)$year)     stop("years in residuals and sr don´t match")
   
   if (dims(res)$unit   != dims(sr)$units)    stop("unit in residuals and sr don´t match")
   if (dims(res)$area   != dims(sr)$area)     stop("area in residuals and sr don´t match")
   if (dims(res)$season != dims(sr)$season)   stop("season in residuals and sr don´t match")
   }
    