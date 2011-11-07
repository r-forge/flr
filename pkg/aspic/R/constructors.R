setMethod('aspic', signature(object='FLQuant'),
  function(object,...)
    {
    args <- list(...)

    # empty object
    object[]<- NA
    dims    <- dims(object)
    res     <- new("aspic")

    catch(res) <- object
    range(res)<-unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)})

setMethod('aspic', signature(object='missing'),
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

    return(aspic(object, ...))
    })

setMethod('aspic', signature(object='character'),
  function(object,...){
    
    if (!file.exists(object)) stop(paste("file", object, "does not exist"))
    
    args <- list(...)

    ### control
    ctrl=aspicControl(object)
    
    ### cpues
    Us=aspicCpues(object)
    
    ### other slots
    ctc=FLQuant(0,dimnames=dimnames(Us[[1]]@catch))
    for (u in Us)
      ctc=ctc+u@catch
    
    res=aspic(catch=as.FLQuant(ctc),ctrl=ctrl)
  
    if ("bio" %in% names(args)){
      if (!file.exists(args[["bio"]])) stop(paste("file", bio, "does not exist"))
      bio        =readASPIC(args[["bio"]])
      res@harvest=bio$harvest%*%bio$fmsy
      res@stock  =bio$biomass%*%bio$bmsy
      
      parNms=dimnames(res@params)$param[dimnames(res@params)$param %in% dimnames(res@ctrl@bounds)$params] 
      
      params(res)[parNms]=res@ctrl@bounds[parNms,"start"]
      
      res@params       =propagate(res@params, dims(res@stock)$iter)
      res@params["msy"]=bio$fmsy*bio$bmsy
      if (model(res)=="schaefer")
        res@params["k"]  =bio$bmsy*2
      
      res@params["b0"]=bio$biomass[,1]/bio$bmsy
      }
      
     # Load given slots
     for(i in names(args)[names(args) %in% names(getSlots("aspic"))])
 			 slot(res, i) <- args[[i]]

    return(res)})

