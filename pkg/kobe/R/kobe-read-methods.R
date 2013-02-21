utils::globalVariables(c("sims","ptsYrs"))

setMethod('kobe',  signature(file='character',method="character"), function(file,method=c("aspic","adapt","bsp","mfcl","ss","sam","vpa"),
                                                                            dir="",what=c("sims","trks","pts","smry","wrms")[1],
                                                                            prob=c(0.75,0.5,0.25),pts=NULL,nwrms=10,...) {
   
    method=tolower(method)
    if (any("2box" == method)) method["2box" == method]="adapt"   
    switch(substr(method[1],1,2),
           ad=kobe2box( file,dir=dir,what=what,prob=prob,pts=pts,nwrms=nwrms,...),
           as=kobeAspic(file,dir=dir,what=what,prob=prob,pts=pts,nwrms=nwrms,...),
           mf=kobeMFCL( file,dir=dir,what=what,prob=prob,pts=pts,nwrms=nwrms,...),
           ss=kobeSS3(  file,dir=dir,what=what,prob=prob,pts=pts,nwrms=nwrms,...))
    })


setMethod('kobe',  signature(file="data.frame",method="missing"),  function(file,method,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),pts=NULL,nwrms=10){
  
  res=llply(object, function(x,what=what,prob=prob,pts=pts,nwrms=nwrms)
    kobeFn(object,what=what,prob=prob,pts=pts,nwrms=nwrms),
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

