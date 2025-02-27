## expands & fills
setGeneric("recycle6d<-", function(object,value){
  standardGeneric("recycle6d<-")})
setMethod("recycle6d<-", signature(object="FLQuant", value="FLQuant"),
	function(object, value) {
    
   if (any(dim(value)>dim(object)))
      stop("dims in 2nd arg can't be greater than those in 1st")
      
   ## dims to expand in value
   nDim<-(1:6)[dim(value)!=pmax(dim(object),dim(value))]

   if (!all(dim(value)[nDim]==1 | dim(value)[nDim]==dim(object)[nDim]))
      stop("dims in 2nd arg can't be greater than 1 and != those in arg 1")
       
   return(sweep(FLQuant(0,dimnames=dimnames(object)), (1:6)[!(1:6 %in% nDim)], value, "+"))})     

fwdSetup<-function(object,flbrp=NULL,nyears=20,start=range(object,"minyear"),stf.control=NULL,...){

    ## Year range for object passed in
    end   =range(object,"maxyear") + nyears
    years =range(object,"maxyear"):end
    object=trim(object,  year=start:range(object,"maxyear"))
    
    ## FLBRP option
    if (!is.null(flbrp)){
      object=expand(object,year=start:end)
  
      slot(object[,ac(years)],"stock.n")[]    <-NA  
      slot(object[,ac(years)],"catch.n")[]    <-NA      
   
      args<-data.frame(e1=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.n",  "discards.n",   "m","mat","harvest"  ,"harvest.spwn","m.spwn"),    
                       e2=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.sel","discards.sel", "m","mat","catch.sel","harvest.spwn","m.spwn"))
                       
#      t.<-FLQuants(mlply(args,function(y,x,br,sk) recycle6Dims(sk[[ac(y)]][[1]],br[[ac(x)]][[1]]),br=flbrp,sk=object))
#      t.<-FLQuants(mlply(args,function(e1,e2,stk,flb) recycle6d(stk[[e1]][[1]],flb[[e2]][[1]]),flb=flbrp,stk=object))
       t. <-FLQuants(mlply(args,function(e1,e2,stk,flb) {recycle6d(stk[[ac(e1)]][[1]])<-flb[[ac(e2)]][[1]]; return(stk[[ac(e1)]][[1]])},stk=object[,ac(years)],flb=flbrp))

      names(t.)<-args[,1]
      object[,ac(years)][[ac(args[,1])]]<-t.
    
    ## STF option
    }else if (!is.null(stf.control)){
      stfCtrl=list(nyears=3, wts.nyears=3, fbar.nyears=NA, 
                   f.rescale=FALSE, arith.mean=TRUE, na.rm=TRUE)
    
      stfCtrl[names(stf.control)]<-stf.control
      if (is.na(stfCtrl$fbar.nyears)) 
         stfCtrl$fbar.nyears<-stfCtrl$wt.nyears
         
      object<-do.call("stf",c(object=object,stfCtrl))}
      
    ## replace any slot passed in as an arg option
    args<-list(...)
    for (slt in names(args)[names(args) %in% names(getSlots(class(object)))]) 
       slot(object, slt)[,ac(years)]<-fn(args[[slt]],slot(object, slt)[,ac(years)])
  
    return(object)}

#unlist(dims(fwdSetup(alb[[1]],flbrp=albBrp[[1]],nyears=23)))

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
    range(res, c('minyear', 'maxyear')) <- unlist(dims(fbar(from))[c('minyear','maxyear')])

    years<-dimnames(slot(res,"m"))$year
    for (i in c("stock.wt","m","mat","harvest.spwn","m.spwn")){
        dimnames(slot(from,i))$year<-dimnames(fbar(from))$year[1]
        slot(res,i)                <- expand(slot(from,i), year=years)
        recycle6d(slot(res,i))     <- slot(from,i)}

    for (i in c("stock.n","catch.n","landings.n","discards.n","harvest"))
        recycle6d(slot(res,i))<-do.call(i,list(from))
        
    recycle6d(   catch.wt(res))<-catch.wt(from)
    recycle6d(discards.wt(res))<-discards.wt(from)
    recycle6d(landings.wt(res))<-landings.wt(from)
    catch(res)                 <-computeCatch(res,"all")
    
    if(validObject(res))
      return(res)
    else
     stop("invalid object created. Please check input object")})
    
    
