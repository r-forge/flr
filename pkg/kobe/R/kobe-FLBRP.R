# #######################################################################################
# ### FLR stuff for Kobe ################################################################
# #######################################################################################

## Light lifting functions ##############################################################

io=function(x,prob=c(0.75,0.5,.025),yrs=NULL,pts=NULL,nwrms=10,what=c("ts","smry","pts","wrms"),proxy="msy"){

    res=as.data.frame(FLQuants(ssb    =x@ssb.obs%/%refpts(x)[proxy,"ssb"],
                               stock  =x@stock.obs%/%refpts(x)[proxy,"biomass"],
                               harvest=x@fbar.obs%/%refpts(x)[proxy,"harvest"]))

    if (is.null(yrs))
       yrs=sort(unique(dimnames(res$year)))
       
    if (is.null(pts))
       pts=yrs
    
    ts  =NULL
    pts.=NULL
    wrms=NULL
    smry=NULL
    
    if ("ts" %in% what){ 
      ssb   =ddply(res,.(year),function(x) quantile(x$ssb,    prob))
      stock =ddply(res,.(year),function(x) quantile(x$stock,  prob))
      hvt   =ddply(res,.(year),function(x) quantile(x$harvest,prob))
      ts=data.frame(melt(ssb,id.vars=c("year","tac")),
                         stock  =melt(hvt,id.vars=c("year","tac"))[,4],
                         harvest=melt(hvt,id.vars=c("year","tac"))[,4])

      names(ts)[3:4]=c("Percentile","ssb","stock")}

    if ("pts" %in% what)
      pts.=subset(res,year %in% pts)[,c("iter","year","ssb","harvest")]
           
    if ("smry" %in% what)
       smry   =ddply(res,  .(year), function(x) data.frame(ssb        =median(x$ssb,       na.rm=T),
                                                           harvest    =median(x$harvest,   na.rm=T),
                                                           red        =mean(x$red,         na.rm=T),
                                                           yellow     =mean(x$yellow,      na.rm=T),
                                                           green      =mean(x$green,       na.rm=T),
                                                           overFished =mean(x$overFished,  na.rm=T),
                                                           overFishing=mean(x$overFishing, na.rm=T)))
    
    if ("wrms" %in% what)
      wrms=subset(res,iter %in% sample(unique(res$iter),nwrms))[,c("iter","year","ssb","harvest")]
                                                         
    return(list(ts=ts,pts=pts.,smry=smry,wrms=wrms))}
 
setMethod('kobeDat', signature(object='FLBRP'),
  function(object,prob=c(0.75,0.5,.025),yrs=NULL,pts=NULL,what=c("ts","pts","smry","wrms"),proxy="msy"){
  
    if (length(object)==1)
       res=io(object,prob=prob,yrs=yrs,pts=pts,proxy=proxy)
    
    if (length(object) >1){
       res=mlply(object, function(x,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what,proxy=proxy)
                                   io2box(x,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what,proxy=proxy),
                      prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what,proxy=proxy)
                 
      res=list(ts  =ldply(res, function(x) x$ts),
               pts =ldply(res, function(x) x$pts),
               smry=ldply(res, function(x) x$smry),
               wrms=ldply(res, function(x) x$wrms))
      }
             
    return(res)})


  

