utils::globalVariables(c("sims","ptsYrs"))

setMethod('kobe',  signature(file='character',method="character"), function(file,method=c("aspic","adapt","bsp","mfcl","ss","sam","vpa"),
                                                                            dir="",what=c("sims","trks","pts","smry","wrms")[1],
                                                                            prob=c(0.75,0.5,0.25),ptYrs=NULL,nwrms=10,...) {
   
    method=tolower(method)
    if (any("2box" == method)) method["2box" == method]="adapt"   
    switch(substr(method[1],1,2),
           ad=kobe2box( file,dir=dir,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms,...),
           as=kobeAspic(file,dir=dir,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms,...),
           mf=kobeMFCL( file,dir=dir,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms,...),
           ss=kobeSS3(  file,dir=dir,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms,...))
    })


setMethod('kobe',  signature(file="data.frame",method="missing"),  function(file,method,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10){
  
  res=llply(object, function(x,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)
    kobeFn(object,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms),
            what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)
  
  res=list(trks=ldply(res, function(x) x$trks),
           pts =ldply(res, function(x) x$pts),
           smry=ldply(res, function(x) x$smry),
           wrms=ldply(res, function(x) x$wrms),
           sims=ldply(res, function(x) x$sims))
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what]) })

kobeFn=function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10,...)
{
  if (!isGeneric("stock"))   setGeneric("stock",   useAsDefault = stock)
  if (!isGeneric("harvest")) setGeneric("harvest", useAsDefault = harvest)
  
  
  setMethod('stock',   signature(object='data.frame'), function(object) object[,"stock"])
  setMethod('harvest', signature(object='data.frame'), function(object) object[,"harvest"])
  
  trks. =NULL
  pts.  =NULL
  smry. =NULL
  wrms. =NULL
  sims. =NULL
  
  pts =NULL #model.frame(FLQuants(stock  =stock(  object)[,ac(ptsYrs)],
            #                     harvest=harvest(object)[,ac(ptsYrs)]),drop=T)
  
  ## trks
  if ("trks" %in% what)
    trks.= NULL #model.frame(FLQuants(stock  =quantile(stock(object),    prob, na.rm=T),
                #                     harvest=quantile(harvest(object),  prob, na.rm=T)),drop=T)
  
  if ("pts" %in% what & !is.null(ptYrs))
    pts. ==pts[pts$year==ptYrs,]
  
  
  if ("smry" %in% what)
    smry. =ddply(kobeP(sims), .(year), function(x) data.frame(stock      =median(stock(object),       na.rm=T),
                                                              harvest    =median(harvest(object),     na.rm=T),
                                                              red        =mean(  x$red,         na.rm=T),
                                                              yellow     =mean(  x$yellow,      na.rm=T),
                                                              green      =mean(  x$green,       na.rm=T),
                                                              overFished =mean(  x$overFished,  na.rm=T),
                                                              overFishing=mean(  x$overFishing, na.rm=T)))
  #           
  if ("wrms" %in% what){          
    wrms =sample(unique(res$iter),nwrms)
    wrms.=sims[sims$iter %in% wrms,]
  }
  
  if ("sims" %in% what)     
     sims. =NULL #ldply(object,model.frame(FLQuants(stock=stock(object),harvest=harvest(object)),drop=T))
  
  res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what])
}
