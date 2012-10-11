# #######################################################################################
# ### aspic stuff for Kobe ##############################################################
# #######################################################################################
ac=as.character

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))

################################################################################
aspicBio. =function(file){
  t.  <-scan(file,skip=4)
  nits<-scan(file,skip=1,nmax=1)
  yrs <-scan(file,skip=2,nmax=2)
  nyrs<-diff(yrs)
  nval<-nyrs*2+3
  
  yrs <-yrs[1]:yrs[2]
  
  b.  <-data.frame(stock  =t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1)  x:(x+y-1)))],year=yrs,               iter=rep(1:nits,each=length(yrs)))
  f.  <-data.frame(harvest=t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)    x:(x+y-1)))],year=yrs[-length(yrs)], iter=rep(1:nits,each=(length(yrs)-1)))
  
  bmsy<-data.frame(bmsy=t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],iter=1:nits)
  fmsy<-data.frame(fmsy=t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],iter=1:nits)
  
  sh   <-merge(b.,  f.,  by=c("iter","year"))
  rf   <-merge(bmsy,fmsy,by=c("iter"))
  
  res=transform(merge(sh,rf),stock=stock/bmsy,harvest=harvest/fmsy)
  
  return(res[do.call("order",res[,c("iter","year")]),c("iter","year","stock","harvest","bmsy","fmsy")])}


aspicPrb. =function(file){
  ## Stuff
  nits<-scan(file,skip=1,nmax=1)
  yrs <-scan(file,skip=2,nmax=2)
  t.  <-scan(file,skip=4)
  ncol<-yrs[2]-yrs[1]+2
  
  ## stock
  first<-rep((1:nits-1)*ncol*2,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
  b.   <-data.frame(stock=t.[first],year=yrs[1]:yrs[2],iter=rep(1:nits,each=length(yrs[1]:yrs[2])))
  
  first<-((1:nits-1)*ncol*2)+1
  bmsy <-data.frame(bmsy=t.[first],iter=1:nits)
  b.   <-merge(b.,bmsy,by="iter")
  
  ## F
  first<-rep((1:nits-1)*ncol*2+ncol,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
  f.   <-data.frame(harvest=t.[first],year=yrs[1]:yrs[2],iter=rep(1:nits,each=length(yrs[1]:yrs[2])))
  
  first<-((1:nits-1)*ncol*2)+ncol+1
  fmsy <-data.frame(fmsy=t.[first],iter=1:nits)
  f.   <-merge(f.,fmsy,by="iter")
  
  res=transform(merge(b.,f.,by=c("iter","year")),stock=stock/bmsy,harvest=harvest/fmsy)
  
  return(res[do.call("order",res[,c("iter","year")]),c("iter","year","stock","harvest","bmsy","fmsy")])}

## Heavy lifting functions ##############################################################
ioAspic=function(bio,prb,prob=c(0.75,0.5,.25),what=c("sims","trks","pts","smry","wrms")[1],nwrms=10){
    
    if (!all(what %in% c("trks","pts","smry","wrms","sims"))) stop("what not in valid options")
    
    if (tolower(getExt(bio)) %in% "bio") bio.=aspicBio.(bio) else stop("Second arg not a .bio file")
    
    if (!is.null(prb)){
      if (tolower(getExt(prb)) %in% "prb") prb.=aspicPrb.(prb) else stop("First  arg not a .prb file")
      res  =subset(rbind(bio.,prb.),!is.na(stock) & !is.na(harvest))}
    else
      res=bio.
    
    trks. =NULL
    pts.  =NULL
    smry. =NULL
    wrms. =NULL
    sims. =NULL
        
    if ("trks" %in% what){ 
      stock  =ddply(res,.(year),function(x) quantile(x$stock,    prob, na.rm=T))
      harvest=ddply(res,.(year),function(x) quantile(x$harvest,  prob, na.rm=T))
      trks.=data.frame(melt(stock,id.vars="year"),harvest=melt(harvest,id.vars="year")[,3])
      names(trks.)[c(2,3)]=c("Percentile","stock")}
    
    if ("pts" %in% what)
      pts.=subset(res,year %in% max(sort(unique(bio.$year))))
    
    if ("sims" %in% what)
      sims.=res
    
    if ("smry" %in% what)
       smry. =ddply(data.frame(res,kobeP(res$stock,res$harvest)),
                           .(year), function(x) data.frame(stock      =median(x$stock,       na.rm=T),
                                                           harvest    =median(x$harvest,     na.rm=T),
                                                           red        =mean(  x$red,         na.rm=T),
                                                           yellow     =mean(  x$yellow,      na.rm=T),
                                                           green      =mean(  x$green,       na.rm=T),
                                                           overFished =mean(  x$overFished,  na.rm=T),
                                                           overFishing=mean(  x$overFishing, na.rm=T)))
    
    if ("wrms" %in% what)
       wrms.=subset(res,res$iter %in% sample(unique(res$iter),nwrms))
    
    return(list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.))
    }
 
setMethod('kobeAspic', signature(object='character'),
  function(object,prb,dir="",prob=c(0.75,0.5,.025),nwrms=10,what=c("sims","trks","pts","smry","wrms")[1]){
    
    bio=paste(dir,object,sep="/")
    prb=paste(dir,prb,   sep="/")
    
    if (length(prb)==1)
       res=ioAspic(bio,prb,prob=prob,nwrms=nwrms)
    
    if (length(prb)>1){
       res=mlply(prb, function(x,bio,prob=prob,nwrms=nwrms,what=what)
                                   ioAspic(bio=bio,prb=x,prob=prob,nwrms=nwrms,what=what),
                      bio=bio,prob=prob,nwrms=nwrms,what=what)
                 
      res=list(trks=ldply(res, function(x) x$trks),
               pts =ldply(res, function(x) x$pts),
               smry=ldply(res, function(x) x$smry),
               wrms=ldply(res, function(x) x$wrms),
               sims=ldply(res, function(x) x$sims))
      }
    
    #if(length(what)==1) return(res[[what]]) else 
    return(res)
    })

# #######################################################################################
# ### FLR aspic stuff for Kobe ##########################################################
# #######################################################################################

## Heavy lifting functions ##############################################################
FLRioAspic=function(prb,bio,prob=c(0.75,0.5,.025),yrs=NULL,pts=NULL,nwrms=10,what=c("ts","smry","pts","wrms")){

    catFlq=function(x,y)
  	as.FLQuant(rbind(as.data.frame(x),as.data.frame(y)))

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
 


  

