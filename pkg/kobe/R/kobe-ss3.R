# #######################################################################################
# ### SS3 stuff for Kobe ################################################################
# #######################################################################################

utils::globalVariables(c("ptYrs","yrs","pts"))

#setMethod('kobeSS3', signature(object='character'),
kobeSS3=function(object,nrows=-1,thin=1,what=c("sims","trks","pts","smry","wrms")[1],
                        prob=c(0.75,0.5,0.25),ptYrs=NULL,nwrms=10){

    require(LaF)

    if (any(grep("derived_posteriors.sso",object)<1)) stop("file not found")

    if (length(object)==1)
       res=ioSS3(object,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin)
    
    if (length(object) >1){
       res=mlply(object, function(x,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what)
                                   ioSS3(x,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what),
                      prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what)
                 
      res=list(ts  =ldply(res, function(x) x$ts),
               pts =ldply(res, function(x) x$pts),
               smry=ldply(res, function(x) x$smry),
               wrms=ldply(res, function(x) x$wrms))
      }
               
    if (length(what)==1) return(res[[what]]) else return(res[what])}


## Heavy lifting functions ##############################################################
ioSS3=function(x,prob=c(0.75,0.5,0.25),yrs=NULL,pts=NULL,nwrms=10,what=c("ts","smry","pts","wrms"),nrows=-1,thin=1){

  
    if (is.null(yrs)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       yrs=nms[substr(nms,1,3)=="Bra"]
       yrs=as.numeric(substr(yrs,8,nchar(yrs)))}

    if (is.null(pts)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       pts=nms[substr(nms,1,3)=="For"]
       pts=min(as.numeric(substr(pts,11,nchar(pts))))-1
       }

    Fs =paste("F",     yrs,sep="_")
    Bs =paste("Bratio",yrs,sep="_")

 ops=options()
 options(warn=-1)
    hd=names(read.csv(x,sep=" ",nrows=1,header=T))

    dat = laf_open_csv(filename=x,column_types=rep("double",length(hd)),
                                   column_names=hd,
                                   sep=" ",
                                   skip=2)

    res <- dat[ ,]
    res =res[,c("Iter",Bs,Fs,"Fstd_MSY")]

    #res=data.frame(apply(read.csv(x,sep=" ",nrows=nrows)[,c("Iter",Bs,Fs,"Fstd_MSY")],2, function(x) as.numeric(ac(x))))
    res=res[seq(1,dim(res)[1],thin),]
    res=melt(res[,c("Iter",Bs,Fs)],id.vars="Iter")
    
    res$year=as.numeric(gsub("Bratio_","",ac(res$variable)))
 options(ops)    
    res$year[is.na(res$year)]=as.numeric(gsub("F_","",ac(res[is.na(res$year),"variable"])))
    res$var=substr(res$variable,1,1)
    res    =data.frame(res[res$var=="B",c("Iter","year","value")],harvest=res[res$var=="F","value"])
    names(res)[c(1,3)]=c("iter","ssb")
    res    =data.frame(res, kobeP(res$ssb,res$harvest))
    res[is.na(res)]=0
   
    ts  =NULL
    pts.=NULL
    wrms=NULL
    smry=NULL
    
    if ("ts" %in% what){ 
      ssb =ddply(res,.(year),function(x) quantile(x$ssb,    prob))
      hvt =ddply(res,.(year),function(x) quantile(x$harvest,prob))
      ts=data.frame(melt(ssb,id.vars="year"),harvest=melt(hvt,id.vars="year")[,3])
      names(ts)[c(2,3)]=c("Percentile","ssb")}

    if ("pts" %in% what & !is.null(ptYrs))
       pts.=res[res$year %in% ptYrs,]
    
    if ("smry" %in% what)
       smry   =ddply(res,  .(year), function(x) data.frame(ssb        =median(x$ssb,       na.rm=T),
                                                           harvest    =median(x$harvest,   na.rm=T),
                                                           red        =mean(x$red,         na.rm=T),
                                                           yellow     =mean(x$yellow,      na.rm=T),
                                                           green      =mean(x$green,       na.rm=T),
                                                           overFished =mean(x$overFished,  na.rm=T),
                                                           overFishing=mean(x$overFishing, na.rm=T)))
    
    if ("wrms" %in% what)
       wrms.=res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","ssb","harvest")]
    
    return(list(ts=ts,pts=pts.,smry=smry,wrms=wrms))}
 

