setMethod('aspic', signature(object='missing'),
          function(...)
          {
            #args <- list(...)
            
            # if no FLQuant argument given, then use empty FLQuant
            #slots <- lapply(args, class)
            #slots <- names(slots)[slots == 'FLQuant']
            
          return(new("aspic"))
          })

setMethod('aspic', signature(object="data.frame"),
    function(object,r=0.25,...){
         
            args <- list(...)
            
            nms=names(object)
         
            res=new("aspic")
            
            if (all(c("year","catch") %in% nms)){
              o=ddply(object, .(year), with, sum(catch))
              res@catch <- FLQuant(o$V1,dimnames=list(year=o$year))
              }
            
            ## CC
            if (all(c("year","catch") %in% nms) & !("index" %in% nms))
               object=transform(object, index=catch/effort)
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
     
            res@bounds[-(1:3),"start"]=daply(res@cpue, .(name), with, 2*mean(index,na.rm=T))/res@bounds["k",  "start"]
            
            res@bounds[,"min"] = res@bounds[,"start"]*0.01
            res@bounds[,"max"] = res@bounds[,"start"]*100.0
            res@bounds[,"fit"]    =1
            res@bounds[,"lambda"] =1
               
            res@bounds["b0","fit"]   =0
            res@bounds[1:3, "lambda"]=0
            
            res@rnd=2062012
            
            return(res)})

setMethod('aspic', signature(object='character'),
          function(object,...)
          {
          return(readAspic(object))
          })
