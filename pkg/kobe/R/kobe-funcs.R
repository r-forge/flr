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

