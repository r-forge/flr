setMethod('aspic', signature(object="data.frame"),
          function(object,idx,r=0.25,...){

            args <- list(...)
            
            nms=names(object)
            
            res=new("aspic")
            
            if (all(c("year","catch") %in% nms)){
              o=ddply(object, .(year), with, sum(catch))
              catch(res) <- FLQuant(o$V1,dimnames=list(year=o$year))
              }
            
            range(res)=unlist(list(minyear=min(object$year), maxyear=max(object$year)))
            
            res@cpue=object
            # Load given slots
            for(i in names(args))
              slot(res, i) <- args[[i]]
            
            nms=dimnames(res@params)
            nms$params=c(nms$params,paste("q",seq(length(unique(object$name))),sep=""))
            
            res@params=FLPar(NA,dimnames=nms)
            
            nms=dimnames(res@bounds)
            nms$params=dimnames(res@params)[[1]]
            nms$params=nms$params
            
            res@bounds=array(as.numeric(NA),dim=laply(nms[-3],length),dimnames=nms[-3])
            
            res@bounds["b0", "start"]=1.0
            res@bounds["msy","start"]=mean(res@catch,na.rm=T)
            res@bounds["k",  "start"]=mean(res@bounds["msy","start"])*4.0/r
            
            K=mean(res@bounds["k",  "start"])
            res@bounds[-(1:3),"start"]=daply(res@cpue, .(name), with, 2*mean(index,na.rm=T)/K)
            
            res@bounds[,"min"] = res@bounds[,"start"]*0.01
            res@bounds[,"max"] = res@bounds[,"start"]*100.0
            res@bounds[,"fit"]    =1
            res@bounds[,"lambda"] =1
            
            res@bounds["b0","fit"]   =0
            res@bounds[1:3, "lambda"]=0
            
            res@rnd=2062012
            
            return(res)})

setMethod('aspic', signature(object='FLQuant',idx="missing"),
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

setMethod('aspic', signature(object='missing',idx="missing"),
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

setMethod('aspic', signature(object='character',idx="missing"),
  function(object,...){
    print(1)
    
    if (!file.exists(object) & tolower(substr(object,1,4)) !="http") stop(paste("file", object, "does not exist"))
    
    res =readAspic(object)
    what=getExt(object)

    u=aspicCpue(object)
    ctc=as.FLQuant(ddply(as.data.frame(FLQuants(llply(u,catch.n))),.(year),function(x) data.frame(data=sum(x$data,na.rm=T))))
   
    catch(res)=ctc
    return(res)
    
    
    args <- list(...)

    ### other slots
    ctc=FLQuant(0,dimnames=dimnames(Us[[1]]@catch))
    for (u in Us)
      ctc=ctc+u@catch
    
    res=aspic(catch=as.FLQuant(ctc),ctrl=ctrl)
    res@name=scan(object,what=character(),sep="\n")[2]
    res@desc="read in from aspic inp file"
 
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

setMethod('aspic', signature(object='FLQuant',idx="FLQuants"),
  function(object,idx,r=0.25){
  
  res=aspic(catch=object)
  nms=dimnames(res@params)
  nms$param=c(nms$param,paste("q",seq(length(idx)),sep=""))
  
  res@params=FLPar(NA,dimnames=nms)
  
  nms=dimnames(res@bounds)
  nms$params=dimnames(res@params)[[1]]
  nms$params=nms$params
  
  res@bounds=as.matrix(array(as.numeric(NA),dim=laply(nms,length),dimnames=nms))
  
  res@bounds["b0", "start"]=1.0
  res@bounds["msy","start"]=mean(catch,na.rm=T)
  res@bounds["k",  "start"]=res@bounds["msy","start"]*4.0/r
  
  res@bounds[-(1:3),"start"]=laply(idx,function(x,catch) mean(x/catch*4/r,na.rm=T),catch=catch(res))

  res@bounds[,"min"] = res@bounds[,"start"]*.01
  res@bounds[,"max"] = res@bounds[,"start"]*100.0
  res@bounds[,"fit"]    =1
  res@bounds[,"lambda"] =1
  
  res@bounds["b0","fit"]   =0
  res@bounds[1:3, "lambda"]=0
  
  res@rnd=2062012
 
  return(res)})

  


