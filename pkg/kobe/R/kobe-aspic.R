# #######################################################################################
# ### aspic stuff for Kobe ##############################################################
# #######################################################################################
ac=as.character

catFlq=function(x,y)
  as.FLQuant(rbind(as.data.frame(x),as.data.frame(y)))

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))

## Heavy lifting functions ##############################################################
ioAspic=function(prb,bio,prob=c(0.75,0.5,.025),yrs=NULL,pts=NULL,nwrms=10,what=c("ts","smry","pts","wrms")){

    require(aspic)

    if (tolower(getExt(bio)) %in% "bio") bio.=readAspic(bio) else stop("First arg not a .bio file")
    if (tolower(getExt(prb)) %in% "prb") prb.=readAspic(prb) else stop("First arg not a .prb file")

    if (is.null(yrs)){
       yrsH=dimnames(bio.$harvest)$year
       yrsP=dimnames(prb.$harvest)$year
 
       yrs=sort(as.numeric(unique(c(yrsH,yrsP))))}
       
    if (is.null(pts))
       pts=max(as.numeric(yrsH))
 
    stock  =catFlq(x=bio.$stock,  y=prb.$stock)
    harvest=catFlq(x=bio.$harvest,y=prb.$harvest)
    
    res=model.frame(mcf(FLQuants("stock"=stock,"harvest"=harvest)))
       
    ts  =NULL
    pts.=NULL
    wrms=NULL
    smry=NULL
    
    if ("ts" %in% what){ 
      stock  =ddply(res,.(year),function(x) quantile(x$stock,    prob))
      harvest=ddply(res,.(year),function(x) quantile(x$harvest,  prob, na.rm=T))
      ts=data.frame(melt(stock,id.vars="year"),harvest=melt(harvest,id.vars="year")[,3])
      names(ts)[c(2,3)]=c("Percentile","stock")}

    if ("pts" %in% what)
      pts.=subset(res,year %in% pts)
           
    if ("smry" %in% what)
       smry   =ddply(data.frame(res,kobeP(res$stock,res$harvest)),
                           .(year), function(x) data.frame(stock      =median(x$stock,     na.rm=T),
                                                           harvest    =median(x$harvest,   na.rm=T),
                                                           red        =mean(x$red,         na.rm=T),
                                                           yellow     =mean(x$yellow,      na.rm=T),
                                                           green      =mean(x$green,       na.rm=T),
                                                           overFished =mean(x$overFished,  na.rm=T),
                                                           overFishing=mean(x$overFishing, na.rm=T)))
    
    if ("wrms" %in% what)
      wrms=subset(res,res$iter %in% sample(unique(res$iter),nwrms))
                                                         
    return(list(ts=ts,pts=pts.,smry=smry,wrms=wrms))}
 
setMethod('kobeAspic', signature(object='character'),
  function(object,bio,prob=c(0.75,0.5,.025),yrs=NULL,pts=NULL,nwrms=10,what=c("ts","pts","smry","wrms")){
  
    if (length(object)==1)
       res=ioAspic(prb,bio,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms)
    
    if (length(object) >1){
       res=mlply(object, function(x,bio,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what)
                                   ioAspic(x,bio=bio,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what),
                      bio=bio,prob=prob,yrs=yrs,pts=pts,nwrms=nwrms,what=what)
                 
      res=list(ts  =ldply(res, function(x) x$ts),
               pts =ldply(res, function(x) x$pts),
               smry=ldply(res, function(x) x$smry),
               wrms=ldply(res, function(x) x$wrms))
      }
             
    return(res)})


  

