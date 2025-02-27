setGeneric('kobe',       function(file,method,...)    standardGeneric('kobe'))

setMethod('kobe', signature(file="FLBRPs",method="missing"),  
          function(file,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){
  if (is.null(pts)) pts=range(file[[1]])["maxyear"]
  
  res=llply(file, function(x,what=what,prob=prob,pts=pts,nwrms=nwrms)
    kobe(model.frame(mcf(FLQuants(stock  =ssb.obs( x)%/%x@refpts[proxy,"ssb"],
                                  harvest=fbar.obs(x)%/%x@refpts[proxy,"harvest"])),drop=T),
            what=what,prob=prob,pts=pts,nwrms=nwrms),
            what=what,prob=prob,pts=pts,nwrms=nwrms)
  
  res=list(trks=ldply(res, function(x) x$trks),
           pts =ldply(res, function(x) x$pts),
           smry=ldply(res, function(x) x$smry),
           wrms=ldply(res, function(x) x$wrms),
           sims=ldply(res, function(x) x$sims))
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what]) })

setMethod('kobe',  signature(file="FLBRP",method="missing"),  
          function(file,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){
            if (is.null(pts)) pts=range(file)["maxyear"]
            
            dat=model.frame(mcf(FLQuants(stock  =ssb.obs( file)%/%refpts(file)[proxy,"ssb"],
                                         harvest=fbar.obs(file)%/%refpts(file)[proxy,"harvest"])),drop=T)
            
            res=kobeFn(dat,what=what,prob=prob,pts=pts,nwrms=nwrms)
            if (length(what)==1)
              return(res[[what]])
            else
              return(res[what])})

setMethod('kobe',  signature(file="FLStock",method="FLPar"),  
          function(file,method,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){
            if (is.null(pts)) pts=range(file)["maxyear"]
            
            dat=model.frame(mcf(FLQuants(stock  =ssb( file)%/%method[,"ssb"],
                                         harvest=fbar(file)%/%method[,"harvest"])),drop=T)
            
            res=kobeFn(dat,what=what,prob=prob,pts=pts,nwrms=nwrms)
            if (length(what)==1)
              return(res[[what]])
            else
              return(res[what])})

setMethod('kobe',  signature(file="data.frame",method="missing"), 
          function(file,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){ 
            kobeFn(file,what=what,prob=prob,pts=pts,nwrms=nwrms)})

kobeFn=function(file,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){         
  object=file
 
  trks. =NULL
  pts.  =NULL
  smry. =NULL
  wrms. =NULL
  sims. =NULL
  
  ## trks
  if ("trks" %in% what){
    
    trks.=rbind(ddply(object,.(year), function(x) data.frame(quantity="stock",  pctl=prob,value=quantile(x$stock,    prob, na.rm=T))),
                ddply(object,.(year), function(x) data.frame(quantity="harvest",pctl=prob,value=quantile(x$harvest,  prob, na.rm=T))))
    
    trks.=transform(trks.,pctl=paste(substr(ac(signif(pctl,2)),3,nchar(ac(signif(pctl,2)))),ifelse(nchar(ac(trks.$pctl))==3,"0",""),"%",sep=""))
    trks.=cast(trks.,year+pctl~quantity,value="value") 
    }
  
  if ("pts" %in% what){
    if (is.null(pts)) pts=max(object[,"year"])
    flag=(object$year==max(object[,"year"]))
    pts. = object[flag,]
    }
  
  if ("smry" %in% what)
    smry. =ddply(kobeP(sims), .(year), function(x) data.frame(stock      =median(stock(object),       na.rm=T),
                                                              harvest    =median(harvest(object),     na.rm=T),
                                                              red        =mean(  x$red,         na.rm=T),
                                                              yellow     =mean(  x$yellow,      na.rm=T),
                                                              green      =mean(  x$green,       na.rm=T),
                                                              overFished =mean(  x$overFished,  na.rm=T),
                                                              overFishing=mean(  x$overFishing, na.rm=T)))
  if ("wrms" %in% what){          
    wrms =sample(unique(res$iter),nwrms)
    wrms.=sims[sims$iter %in% wrms,]
  }
  
  if ("sims" %in% what)     
    sims. =object
  
  res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
  
  res}

setMethod('kobe',  signature(file="FLStocks",method="FLPar"),  
          function(file,method,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),
                   pts =NULL,nwrms=10){
                        
            res=llply(file,function(x,method,what,prob,pts,nwrms) kobe(x,method,what,prob,pts,nwrms),
                  method=method,what=what,prob=prob,pts=pts,nwrms=nwrms)  

            if (length(what)==1)
              return(ldply(res, function(x) x))  
            
            res=list(trks=ldply(res, function(x) x$trks),
                     pts =ldply(res, function(x) x$pts),
                     smry=ldply(res, function(x) x$smry),
                     wrms=ldply(res, function(x) x$wrms),
                     sims=ldply(res, function(x) x$sims))
            
            return(res[what])})

setMethod('kobe',  signature(file="FLStocks",method="FLPars"),  
          function(file,method,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),
                   pts =NULL,nwrms=10){
            
            res=llply(names(file),function(x,file,method,what,prob,pts,nwrms) 
                  kobe(file[[x]],method[[x]],what,prob,pts,nwrms),
                    file=file,method=method,what=what,prob=prob,pts=pts,nwrms=nwrms)   
            names(res)=names(file)
            if (length(what)==1)
              return(ldply(res, function(x) x)) 
            
            res=list(trks=ldply(res, function(x) x$trks),
                     pts =ldply(res, function(x) x$pts),
                     smry=ldply(res, function(x) x$smry),
                     wrms=ldply(res, function(x) x$wrms),
                     sims=ldply(res, function(x) x$sims))
            
          
              return(res[what])})

