# #######################################################################################
# ### SS3 stuff for Kobe ################################################################
# #######################################################################################
ac=as.character


nmsRef <- c("iter", 
            "fmsy",   "ymsy",   "yrmsy",    "srmsy",   "sprmsy",  "ssbmsy",
            "fmax",             "yrmax",    "srmax",   "sprmax",  "ssbmax",
            "f0.1",             "yr0.1",    "sr0.1",   "spr0.1",  "ssb0.1",
            "f20",              "yr20",     "sr20",               "ssb20",
            "f30",              "yr30",     "sr30",               "ssb30",
            "f40",              "yr40",     "sr40",               "ssb40",
            "f90max", "y90max", "yr90max",  "sr90max",            "ssb90max",
            "f75max", "y75max", "yr75max",  "sr75max",            "ssb75max")

## Heavy lifting functions ##############################################################
readKobe2box=function(dir,proxy=c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max")[1]){

  rfp=read.table(paste(dir,"BENCH-1.OUT",sep="/"),header=F,skip=1,col.names=nmsRef)[,c("iter",proxy,paste("ssb",substr(proxy,2,nchar(proxy)),sep=""))]
 
  bio=read.table(paste(dir,"BIO_f-1.OUT",sep="/"),header=F,skip=1)
  names(bio)=c("tac","iter",seq(dim(bio)[2]-2))
  bio=merge(bio,rfp)
  bio=melt(bio,id.vars=c("tac",names(rfp)),variable_name="year")
  names(bio)[6]="stock"
  
  hvt=read.table(paste(dir,"Fapex-1.OUT",sep="/"),header=F,skip=1)
  names(hvt)=c("tac","iter",seq(dim(hvt)[2]-2))
  hvt=merge(hvt,rfp)
  hvt=melt(hvt,id.vars=c("tac",names(rfp)),variable_name="year")
  names(hvt)[6]="harvest"
  
  res=merge(bio,hvt[,c("tac","iter","year","harvest")])
  res=transform(res,stock  =stock/res[,paste("ssb",substr(proxy,2,nchar(proxy)),sep="")],
                    harvest=harvest/res[,proxy])
  
  res=res[do.call(order,res[,c("tac","iter","year")]),]
  
  dimnames(res)[[1]]=sort(as.numeric(dimnames(res)[[1]]))
    
  return(res)}

io2box=function(x,proxy=c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max")[1],prob=c(0.75,0.5,.25),what=c("sims","trks","pts","smry","wrms")[1],nwrms=10,ptYrs=NULL){
  
  if (!all(what %in% c("trks","pts","smry","wrms","sims"))) stop("what not in valid options")
  
  res=readKobe2box(x,proxy)

  ts  =NULL
  pts.=NULL
  wrms=NULL
  smry=NULL
   
    if ("sims" %in% what)
      sims.=res
  
    if ("ts" %in% what){ 
      ssb =ddply(res,.(year,tac),function(x) quantile(x$ssb,    prob))
      hvt =ddply(res,.(year,tac),function(x) quantile(x$harvest,prob))
      ts=data.frame(melt(ssb,id.vars=c("year","tac")),harvest=melt(hvt,id.vars=c("year","tac"))[,4])
      names(ts)[3:4]=c("Percentile","ssb")}

    if ("pts" %in% what)
      if (is.null(yrPts)) stop("need to specify 'ptYrs'") else pts.=subset(res,year %in% pts)[,c("iter","year","ssb","harvest")]
           
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
                                                         
    return(list(ts=ts,pts=pts.,smry=smry,wrms=wrms,sims=sims.))}
 
setMethod('kobe2box', signature(object='character'),
          function(object,prob=c(0.75,0.5,.025),what=c("sims","trks","pts","smry","wrms")[1],nwrms=10,ptYrs=NULL){
            
    if (length(object)==1)
       res=io2box(dir,prob=prob,nwrms=nwrms,ptYrs=ptYrs)
            
    if (length(object)>1){
       res=mlply(object, function(x,prob=prob,nwrms=nwrms,what=what,ptYrs=ptYrs)
                 io2box(x,prob=prob,nwrms=nwrms,what=what,ptYrs=ptYrs),
                        prob=prob,nwrms=nwrms,what=what,ptYrs=ptYrs)
              
              res=list(trks=ldply(res, function(x) x$trks),
                       pts =ldply(res, function(x) x$pts),
                       smry=ldply(res, function(x) x$smry),
                       wrms=ldply(res, function(x) x$wrms),
                       sims=ldply(res, function(x) x$sims))
       }
            
       #if(length(what)==1) return(res[[what]]) else 
       return(res)
       })


