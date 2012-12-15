utils::globalVariables(c("xFac","yFac"))

ac=as.character

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))


## Calculates frequency of points in squares
kobeFreq=function(x,y,x.n=11,y.n=x.n,na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  df=data.frame(x=x,y=y)
  df=data.frame(df,xFac=cut(df$x,seq(min(df$x),max(df$x),length.out=x.n)),
                yFac=cut(df$y,seq(min(df$y),max(df$y),length.out=y.n)))
  
  c.=ddply(data.frame(df,count=1),.(xFac,yFac), function(x) count(x$count))[,c("xFac","yFac","freq")]
  
  p.=merge(df,c.,by=c("xFac","yFac"))[,c("x","y","freq","xFac","yFac")]
  
  return(p.[order(p.$freq),])}

## calculates density of points
kobeDens=function(x,y,n=11,na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  
  dat=data.frame(x=x,y=y,n=n)
  f1 =with(dat, kde2d(x,y,n=n)) 
  f2 =data.frame(expand.grid(x=f1$x, y=f1$y), z=as.vector(f1$z))
  
  return(f2)}

## calculates 2D probabilities
kobeProb=function(x,y,prob=c(0.5, 0.75,0.95),na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
    
  tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob)
  
  
  prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))
  
  return(prb)}

setMethod('kobe',  signature(file="data.frame",method="missing"), 
          function(file,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10){ 
            kobeFn(file,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)})

kobeFn=function(file,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10){         
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
            
            if ("pts" %in% what & !is.null(ptYrs))
              pts. =object[object$year==ptYrs,]
            
            
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


