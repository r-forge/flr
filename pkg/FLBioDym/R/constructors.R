# constructors - constructor methods for FLBioDym
# FLBioDym/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setGeneric('FLBioDym', function(object, ...)
		standardGeneric('FLBioDym'))

setMethod('FLBioDym', signature(object='FLQuant'),
  function(object,model="pellat",...)
    {
    args <- list(...)

    # empty object
    object[]<- NA
    dims    <- dims(object)
    res     <- new("FLBioDym")

    catch(res)<-object
    index(res)<-object
    range(res)<-unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))

    res@model <-model
    res@params<-setParams(model)

    if ("p" %in% dimnames(params(albSP))$params) if (is.na(res@params["p",])) res@params["p",]<-2
    if (is.na(res@params["b0",])) res@params["b0",]<-1

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
    }
)

setMethod('FLBioDym', signature(object='missing'),
  function(...)
    {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant()
    else
      object <- args[[slots[1]]]

    return(FLBioDym(object, ...))
    }
)

is.FLBioDym <- function(x)
	return(inherits(x, "FLBioDym"))
