# fwd.R - 
# FLBioDym/R/fwd.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# fwd(FLBioDym) {{{
setMethod("fwd", signature(object="FLBioDym", fleets = "missing"),
  function(object, catch=NULL, harvest=NULL) {

    ## catch or harvest?
    ctcNull=is.null(catch) 
    if(ctcNull & is.null(harvest))
      stop("must supply catch or harvest")
      
    ## check year range
    if (!ctcNull) {
      if (!(all(dimnames(catch)$year %in% dimnames(catch(object))$year)))
        stop("years in catch & stock dont match")
      catch(object)[,dimnames(catch)$year] <- catch
      yrs <- dimnames(catch)$year
    } else {
      if (!(all(dimnames(harvest)$year %in% dimnames(catch(object))$year)))
        stop("years in harvest & stock dont match")
      yrs <- dimnames(harvest)$year
    }

      ## B0 in year 1?
      if (as.numeric(yrs[1]) == range(object,"minyear"))
        stock(object)[,ac(range(object,"minyear"))] <-
          params(object)["K"] * params(object)["b0"]

      ## maxyear
      if (max(as.numeric(yrs)) == range(object,"maxyear"))
        stock(object) <- window(stock(object),end=range(object,"maxyear")+1)

     nits=max(dims(object)$iter,dims(harvest)$iter)
     if (nits>1){ 
               catch(object)=propagate(catch(object),nits)
               stock(object)=propagate(stock(object),nits)
               params(object)=propagate(params(object),nits)}   
    
      for(y in as.numeric(yrs)) {
        if (ctcNull)
           catch(object)[,ac(y)] <- stock(object)[,ac(y)]*harvest[,ac(y)]
         stock(object)[,ac(y+1)] <- stock(object)[,ac(y)] - catch(object)[,ac(y)] + sp(object)[, ac(y)]
         }

    stock(object)[stock(object) < 0] = 0

    return(object)
  }
) # }}}
