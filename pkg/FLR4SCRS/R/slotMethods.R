setGeneric("z", function(object, ...){
	standardGeneric("z")})
setMethod("z", signature("FLStock"), function(object) harvest(object)+m(object))
setMethod("z", signature("FLBiol"),  function(object) harvest(object)+m(object))

setGeneric("survivors", function(object, ...){
	standardGeneric("survivors")})
setMethod("survivors", signature("FLStock"), function(object) stock.n(object)*exp(-z(object)))

setGeneric("quantCumSum", function(x, ...)
  standardGeneric("quantCumSum"))
setMethod('quantCumSum', signature(x='FLQuant'), function(x, MARGIN=2:6,na.rm=TRUE) {
   dat =as.data.frame(x)
   dmns=dimnames(x)
   res =FLQuant(unlist(tapply(dat[,"data"],dat[,MARGIN],cumsum)),dimnames=dmns)

   return(res)})
   
setMethod('expand', signature(x='FLArray'),
 function(x,fill.1s=FALSE,...)
  {
    args <- list(...)
    nargs <- names(args)

    # dimension names
    qnames <- names(dimnames(x))

    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]

    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]

    # turn into characters
    select <- lapply(select, as.character)

    # match specified dimensions and dimnames
    dimnames <- dimnames(x)
    dimnames[names(select)] <- select

    # output object
    res <- new(class(x), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x))

    # list for assignment of x data
    dimnames <- dimnames(x)
    names(dimnames) <- c('i', 'j', 'k', 'l', 'm', 'n')

    res<-do.call('[<-', c(list(x=res, value=x), dimnames))

    ## if the original dim only had a length of 1 then recycle that dim
    if (fill.1s){
       oldDims  <-unlist(dims(x))
       newDims  <-unlist(dims(res))

      ## check that original dim(s) were 1
      if (all(oldDims[names(newDims[newDims!=oldDims])]==1))
         res[]<-rep(c(x),prod(newDims[names(newDims[newDims!=oldDims])]))}
      
    return(res)})
  
recycleFLQuantOverYrs<-function(object,flq){
   ### if averaged over years then expand years
   if (dim(flq)[2]==1 & dim(object)[2]>=1){
      object[]<-rep(c(flq),dim(object)[2])
      return(object)} else
      return(flq)}

setGeneric("wt<-", function(object,...,value){
	standardGeneric("wt<-")})
setMethod("wt<-", signature(object="FLStock", value="FLQuant"),
	function(object, ..., value) {

		stock.wt(   object)<-recycleFLQuantOverYrs(stock.wt(   object),value)
    catch.wt(   object)<-recycleFLQuantOverYrs(catch.wt(   object),value)
    discards.wt(object)<-recycleFLQuantOverYrs(discards.wt(object),value)
    landings.wt(object)<-recycleFLQuantOverYrs(landings.wt(object),value)

		return(object)})

setGeneric("FLQuants<-", function(object,value){
	standardGeneric("FLQuants<-")})
setMethod("FLQuants<-", signature(object="FLComp", value="FLQuants"),
	function(object, value) {

    cls<-class(object)
    nms<-names(getSlots(cls)[getSlots(cls)=="FLQuant"])
    
    for (i in names(value)[names(value) %in% nms])
       slot(object,i)<-recycleFLQuantOverYrs(slot(object,i),value[[i]])
    

		return(object)})

setGeneric("sel<-", function(object,value){
	standardGeneric("sel<-")})
setMethod("sel<-", signature(object="FLStock", value="FLQuants"),
	function(object, value) {

   harvest(   object)<-recycleFLQuantOverYrs(harvest(   object),value[["harvest"]])
   catch.n(   object)[]<-NA
   discards.n(object)<-recycleFLQuantOverYrs(discards.n(object),value[["discards.n"]])
   landings.n(object)<-recycleFLQuantOverYrs(landings.n(object),value[["landings.n"]])
      
   return(object)})

setGeneric("sopCorrect", function(object,...){
	standardGeneric("sopCorrect")})
setMethod("sopCorrect", signature(object="FLStock"),
	function(object,wt=TRUE) {
   if (wt){
     catch.wt(   object)<-sweep(catch.wt(object),   2:6,sop(object,"catch"),   "*")
     landings.wt(object)<-sweep(landings.wt(object),2:6,sop(object,"landings"),"*")
     discards.wt(object)<-sweep(discards.wt(object),2:6,sop(object,"discards"),"*")} else {

     catch.n(   object)<-sweep(catch.n(object),   2:6,sop(object,"catch"),   "*")
     landings.n(object)<-sweep(landings.n(object),2:6,sop(object,"landings"),"*")
     discards.n(object)<-sweep(discards.n(object),2:6,sop(object,"discards"),"*")}

     ## now adjust biomass estimates
     landings(   object)<-computeLandings(object)
     discards(   object)<-computeDiscards(object)
     catch(      object)<-computeCatch(   object,"all")

   return(object)})
