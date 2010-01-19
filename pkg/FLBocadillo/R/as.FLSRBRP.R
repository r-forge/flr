#### put in FLBocalliu
setMethod("as.FLSR", signature(object="FLBRP"),
  function(object, ...)
	  {
	  rec.age  <-range(object,"min")
	  recYrCls <-as.numeric(dimnames(rec.obs(object))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(object))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR object incorprating rec.age
    rec <- rec.obs(object)[,ac(recYrCls)]
    ssb <- ssb.obs(object)[,ac(ssbYrCls)]

    # create the FLSR object
    sr = FLSR(name    =object@name,
		        rec       =rec,
             ssb      =ssb,
             fitted   =FLQuant(dimnames=dimnames(rec), units=units(rec)),
		         residuals=FLQuant(dimnames=dimnames(rec)),
             desc     ="'rec' and 'ssb' slots obtained from a 'FLBRP' object", ...)

    validObject(sr)
    return(sr)
    })
