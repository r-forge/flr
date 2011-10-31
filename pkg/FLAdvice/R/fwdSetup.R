## expands & fills
setGeneric("recycle6d<-", function(object,value){
  standardGeneric("recycle6d<-")})
setMethod("recycle6d<-", signature(object="FLQuant", value="FLQuant"),
	function(object, value) {
  
   if (any(dim(value)[-6]>dim(object)[-6]))
      stop("dims in 2nd arg can't be greater than those in 1st")
   if (dims(value)$iter>1 & dims(object)$iter==1)
      object=propagate(object,dims(value)$iter)
    
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
   
    object=CheckNor1(object)
    
    ## FLBRP option
    if (!is.null(flbrp)){
      object=expand(object,year=start:end)
  
      slot(object[,ac(years)],"stock.n")[]    <-NA  
      slot(object[,ac(years)],"catch.n")[]    <-NA      
   
      args<-data.frame(e1=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.n",  "discards.n",   "m","mat","harvest"  ,"harvest.spwn","m.spwn"),    
                       e2=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.sel","discards.sel", "m","mat","catch.sel","harvest.spwn","m.spwn"))
                       
#      t.<-FLQuants(mlply(args,function(y,x,br,sk) recycle6Dims(sk[[ac(y)]][[1]],br[[ac(x)]][[1]]),br=flbrp,sk=object))
#      t.<-FLQuants(mlply(args,function(e1,e2,stk,flb) recycle6d(stk[[e1]][[1]],flb[[e2]][[1]]),flb=flbrp,stk=object))
       t. <-FLQuants(mlply(args,function(e1,e2,stk,flb) {cat(ac(e1),ac(e2),"\n"); recycle6d(stk[[ac(e1)]][[1]])<-flb[[ac(e2)]][[1]]; return(stk[[ac(e1)]][[1]])},stk=object[,ac(years)],flb=flbrp))

      names(t.)<-args[,1]

      args=cbind(args[,1:2],ldply(ac(args[,1]),function(x,a,b)  data.frame("O"=dims(a[[x]][[1]])$iter,"I"=dims(b[[x]])$iter),a=object,b=t.))
      
      t..=FLQuants(mlply(args, function(e1,e2,O,I,x) if (I>O) propagate(x[[ac(e1)]][[1]],I) else x[[ac(e1)]][[1]],x=object))
      names(t..)<-args[,1]
      object[[ac(args[,1])]]=t..

      
      object[,ac(years)][[ac(args[,1])]]=t.

      
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

setGeneric("fwdWindow", function(x,y,...){
  standardGeneric("fwdWindow")})

setMethod('fwdWindow', signature(x='FLStock',y="FLBRP"),
  function(x,y,start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1,...){
      object =qapply(x, window, start=start, end=end, extend=extend, frequency=frequency)
 
      x@range["minyear"] <- start
     	x@range["maxyear"] <- end
      
      yr1 =dimnames(m(object))$year
      yr2 =dimnames(m(x))$year
      yrs =yr1[!(yr1 %in% yr2)]
 
      object=CheckNor1(object)

    
      slot(object[,yrs],"stock.n")[]    <-NA  
      slot(object[,yrs],"catch.n")[]    <-NA      
    
      args<-data.frame(e1=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.n",  "discards.n",   "m","mat","harvest"  ,"harvest.spwn","m.spwn"),    
                       e2=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.sel","discards.sel", "m","mat","catch.sel","harvest.spwn","m.spwn"))
                       
       t. <-FLQuants(mlply(args,function(e1,e2,stk,flb) {cat(ac(e1),ac(e2),"\n"); recycle6d(stk[[ac(e1)]][[1]])<-flb[[ac(e2)]][[1]]; return(stk[[ac(e1)]][[1]])},stk=object[,yrs],flb=y))

       names(t.)<-args[,1]

       args=cbind(args[,1:2],ldply(ac(args[,1]),function(x,a,b)  data.frame("O"=dims(a[[x]][[1]])$iter,"I"=dims(b[[x]])$iter),a=object,b=t.))
      
       t..=FLQuants(mlply(args, function(e1,e2,O,I,x) if (I>O) propagate(x[[ac(e1)]][[1]],I) else x[[ac(e1)]][[1]],x=object))
       names(t..)<-args[,1]
       object[[ac(args[,1])]]=t..

       object[,yrs][[ac(args[,1])]]=t.

                       
     ## replace any slot passed in as an arg option
     args<-list(...)
     for (slt in names(args)[names(args) %in% names(getSlots(class(object)))]) 
        slot(object, slt)[,ac(years)]<-fn(args[[slt]],slot(object, slt)[,ac(years)])
 
     return(object)})

setMethod('fwdWindow', signature(x='FLStock',y="character"),
  function(x,y,start=dims(x)$minyear, end=dims(x)$maxyear,
           control=list(nyears=3, wts.nyears=3, fbar.nyears=NA,f.rescale=FALSE, arith.mean=TRUE, na.rm=TRUE), ...){
      if (y!="stf") stop("Only stf for now")
      
      x@range["minyear"] <- start
      x@range["maxyear"] <- end
      
      x=CheckNor1(x)
      
      stfCtrl=list(nyears=3, wts.nyears=3, fbar.nyears=NA,f.rescale=FALSE, arith.mean=TRUE, na.rm=TRUE)
    
      stfCtrl[names(control)]<-control
      if (is.na(stfCtrl$fbar.nyears)) 
         stfCtrl$fbar.nyears<-stfCtrl$wt.nyears
 
      object<-do.call("stf",c(object=object,stfCtrl))
      
      ## replace any slot passed in as an arg option
      args<-list(...)
      for (slt in names(args)[names(args) %in% names(getSlots(class(object)))]) 
        slot(object, slt)[,ac(years)]<-fn(args[[slt]],slot(object, slt)[,ac(years)])
  
    return(object)})

if (FALSE){
  tmp=FLStock(m=FLQuant(0.1,dimnames=list(age=1:5,year=2001:2020)))
  units(harvest(tmp))="f"
  tmp2=fwdWindow(tmp,FLBRP(tmp),end=2030)
  }




    
    
