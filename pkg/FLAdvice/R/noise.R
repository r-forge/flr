# noise generator
# Ranta and Kaitala 2001 Proc. R. Soc.

# vt = B * vt-1 + s * sqrt(1 - B^2)

# s is normally distributed random variable with mean = 0, variance = 1
# B is the autocorrelation parameter

#spectra(.5*(1+noise(30,.8)*.3))

noise <- function(len,B=0,trun=NA){

    x <- rep(0, len+1) # going to hack off the first value at the end
    s <- rnorm(len,mean=0,sd=1)
    
    for(i in 1:len){
      x[i+1] <- B * x[i] + s[i] * sqrt(1 - B^2)
      if(!is.na(trun)){
        if (x[i+1] > (1-trun))  x[i+1] <- ( 1-trun)
        if (x[i+1] < (-1+trun)) x[i+1] <- (-1+trun)
          }
      }
    
    x<-x[-1]
    
    return(x)}

noise. <- function(s,len=length(s),B=0,trun=NA){
  
    x <- rep(0, len+1) # going to hack off the first value at the end
    
    for(i in 1:len){
      x[i+1] <- B * x[i] + s[i] * sqrt(1 - B^2)
      if(!is.na(trun)){
        if (x[i+1] > (1-trun))  x[i+1] <- ( 1-trun)
        if (x[i+1] < (-1+trun)) x[i+1] <- (-1+trun)
          }
      }
    
    x<-x[-1]
    
    return(x)}


#white <- noise(10000,B=0)
#red <- noise(100000,B=0.8)
#blue <- noise(1000,B=-0.8)

rnoise <- function(n, mean=0, sd=1, rho=0, what="rnorm"){
    s    =do.call(what,list(n=n,mean=mean,sd=sd))
    s[,1]=do.call(what,list(n=n,mean=mean[,1],sd=sd*(1-rho^2)))
    x=s 
    for(i in seq(dim(s)[2])[-1])
      x[,i] <- rho * x[,i-1] + s[,i-1] * sqrt(1-rho^2)
    
    return(x)}
    

## cohort effects
cEff=function(stk,par,sigma,rho=0,var="k"){
  
  cv=sigma #getS(sigma,rho)
  dmns       =dimnames(par)
  dmns$cohort=dimnames(FLCohort(m(stk)))$cohort
  parC       =FLPar(rep(c(par),length(dmns$cohort)),dimnames=dmns[c(1,3,2)])
  units(parC)=""  
  
  dev        =log(rlnorm(length(dmns$cohort),0,cv))   
  for(i in 2:(length(dev)))
       dev[i]=dev[i]+dev[i-1]*rho
  parC[var]=parC[var]*exp(dev)
  
  tmp=len2wt(parC,vonB(parC[c("linf","t0","k")],ages(FLCohort(m(stk)))))
  res=window(as(tmp,"FLQuant"),start=1,end=dims(m(stk))$year)
  
  res}


