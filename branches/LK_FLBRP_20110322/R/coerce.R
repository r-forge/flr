# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 11 Oct 2010 10:19
# $Id$

# as.FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from)
	{

    sr <- FLSR(name=from@name, desc = "created from a 'FLBRP' object",
      rec=rec.obs(from), ssb=ssb.obs(from))
    model(sr) <- SRModelName(model(from))
    params(sr) <- params(from)
    
    if(validObject(sr))
      return(sr)
    else
      stop("invalid object created. Please check input object")})

recycleFLQuantOverYrs<-function(object,flq){
   ### if averaged over years then expand years
   if (dim(flq)[2]==1 & dim(object)[2]>=1){
      object[]<-rep(c(flq),dim(object)[2])
      return(object)} else
      return(flq)}

setAs('FLBRP', 'FLStock',
  function(from){

    years <- dimnames(fbar(from))$year
    flq<-landings.n(from)
    flq[]<-NA
    res <- FLStock(flq,
      # TODO extend slots for years: check all slots present
      name=name(from))
      #, desc=paste("Created by coercion from 'FLBRP'", desc(from)))

    # range
    range(res)<-range(from)
    range(res, c('minyear', 'maxyear')) <- unlist(dims(fbar(from))[c('minyear',
      'maxyear')])

    years<-dimnames(slot(res,"m"))$year
    for (i in c("stock.wt","m","mat","harvest.spwn","m.spwn")){
        dimnames(slot(from,i))$year<-dimnames(fbar(from))$year[1]
        slot(res,i)                <- expand(slot(from,i), year=years)
        slot(res,i)                <- recycleFLQuantOverYrs(slot(res,i),slot(from,i))}

    for (i in c("stock.n","catch.n","landings.n","discards.n","harvest")){
        print(i)
        print(slot(res,i))
        print(do.call(i,list(from)))
        slot(res,i)<- recycleFLQuantOverYrs(slot(res,i),do.call(i,list(from)))}
        
    catch.wt(res)   <-recycleFLQuantOverYrs(catch.wt(res),catch.wt(from))
    discards.wt(res)<-recycleFLQuantOverYrs(discards.wt(res),discards.wt(from))
    landings.wt(res)<-recycleFLQuantOverYrs(landings.wt(res),landings.wt(from))
    catch(res)      <-computeCatch(res,"all")
    
    if(validObject(res))
      return(res)
    else
     stop("invalid object created. Please check input object")})

setMethod("as.data.frame", 
signature(x="FLBRP", row.names="ANY", optional="character"),
  function(x, row.names=NULL, optional)
  {
    if(any(c('model', 'params', 'refpts', 'name', 'desc', 'range') %in% optional))
      stop("Only 'FLQuant' slots can be converted")
    df <- function(x, slots, names=slots)
    {
      res <-FLQuants(mlply(slots, function(x,fl)
        do.call(x,list(fl)), fl=x))

      names(res)<-slots
      return(
          as.data.frame(res, row.names=row.names)
          )
    }

    return(df(x,optional))
  }
)

setMethod("as.FLSR", signature(object="FLBRP"),
  function(object, ...){

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
	           rec     =rec,
             ssb     =ssb,
             desc    ="'rec' and 'ssb' slots obtained from a 'FLBRP' object", ...)

    validObject(sr)
    return(sr)
    })
