setGeneric('FLAssess', function(object, ...)
		standardGeneric('FLAssess'))

setMethod('FLAssess', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
    {
    args <- list(...)

    # empty object
    object[] <- NA

    dims <- dims(object)

    res <- new("FLAssess",
        catch.n=object,stock.n=object,harvest=object,
        index=FLQuants(object),index.res=FLQuants(object),index.hat=FLQuants(object),index.var=FLQuants(object),
        range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
	     		                  minyear=dims$minyear, maxyear=dims$maxyear,
                            minfbar=dims$min, maxfbar=dims$max)))
    # Load given slots
  	for(i in names(args)) {

			if (i %in% c("index","index.res","index.hat","index.var") & is(args[[i]],"FLQuant"))
			   slot(res, i) <- FLQuants(args[[i]])
      else
			   slot(res, i) <- args[[i]]
    }

    return(res)
  }
)

setMethod('FLAssess', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    
    if(length(slots) == 0)
      object <- FLQuant()
    else {
        object <- args[[slots]]}
    
    return(FLAssess(object, ...))
  }
) # }}}

FLAssess(index=cpue)