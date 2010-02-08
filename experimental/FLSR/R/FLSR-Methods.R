sprFunc<-function(type,spr,a=NULL,b=NULL,c=NULL,d=NULL){
      # SSB as function of ssb/rec
      switch(type,
        "bevholt"  =a*(spr)-b,
        "ricker"   =log(a*spr)/b,
        "cushing"  =(1/(a*spr))^(1/(b-1)),
        "shepherd" =b*(a*spr-1)^(1/c),
        "segreg"   =ifelse(ssb <= b, a/(spr), 0),
        "mean"     =a/(spr),
        "dersh"    =ssb*a*(1-b*c*ssb)^c,
        "pellat"   =1/(a/ssb-a/ssb*(ssb/b)^c),
        "bevholtD" =NULL,
        "rickerdD" =NULL,
        "shepherdD"=NULL)}

srrFunc<-function(type,ssb=NULL,spr=NULL,a=NULL,b=NULL,c=NULL,d=NULL){
    #recruits as function of ssb or ssb/red
    if (is.null(ssb) & !is.null(spr))
       ssb<-sprFunc(type,spr,a,b,c)

      switch(type,
          "bevholt"  =a*ssb/(b+ssb),
          "ricker"   =a*ssb*exp(-b*ssb),
          "cushing"  =a*ssb^b,
          "shepherd" =a*ssb/(1+(ssb/b)^c),
          "segreg"   =ifelse(ssb<=b,a*ssb,a*b),
          "mean"     =a,
          "dersh"    =a*(1-b*c*ssb)^(1/c),
          "pellat"   =a*(1-(ssb/b)^c),
          "bevholtD" =a*ssb/(b+ssb),
          "rickerD"  =a*ssb*exp(-b*ssb),
          "shepherdD"=a*ssb^2/(1+(ssb/b)^c))}

sv<-function(type,spr0,a,b=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      v=sprFunc(type,spr0,a=a,b=b,c=c,d=d)
      s=srrFunc(type,ssb=v*.2,a=a,b=b,c=c,d=d)/srrFunc(type,ssb=v,a=a,b=b,c=c,d=d)

      res<-c(s,v)
      names(res)<-c("s","v")
      return(res)}

ab<-function(type,spr0,s=NULL,v=NULL,c=NULL,d=NULL){
      # converts a&b parameterisation into steepness & vergin biomass
      switch(type,
        "bevholt" ={a<-(v+(v-s*v)/(5*s-1))/spr0;             b<-(v-s*v)/(5*s-1)},
        "ricker"  ={b<-log(5*s)/(v*0.8);                     a<-exp(v*b)/spr0},
        "cushing" ={b<-log(s)/log(0.2);                      a<-(v^(1-b))/(spr0)},
        "shepherd"={b<-v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c));   a<-((v/b)^c+1)/spr0},
        "mean"    ={a<-v/spr0;                               b<-NULL},
        "segreg"  ={a<-5*s/spr0;                             b<-v/(a*spr0)},
        "pellat"  =stop("not done yet"),
        "dersch"  =stop("not done yet"),
        "bevholtD"  =stop("not done yet"),
        "rickerD"  =stop("not done yet"),
        "shepherD"  =stop("not done yet"))

        res<-cbind(a,b)
        return(res)}

setGeneric("sigma", function(obj, ...){
  standardGeneric("sigma")})
setMethod('sigma',
  signature(obj='FLQuant'),
    function(obj,hat=rep(0,length(obj))){
       ## calculates sigma squared for use in concentrated liklihood
       SS<-sum((obj-hat)^2,na.rm=T)

       return((SS/length(hat))^0.5)})

setGeneric("rSq", function(obs,hat){
  standardGeneric("rSq")})
setMethod('rSq',
  signature(obs='FLQuant',hat='FLQuant'),
    function(obs,hat=rep(0,length(obj))){
       ## calculates R squared
       mn   <-mean(obs)
       mnHat<-mean(hat)
       SStot<-sum((obs-mn)^2)
       SSreg<-sum((hat-mnHat)^2)
       SSerr<-sum((obs-hat)^2)

       res  <-1-SSerr/SStot

       return(res)})

setGeneric("loglAR1", function(obs,hat,rho,sigma){
  standardGeneric("loglAR1")})
setMethod('loglAR1',
  signature(obs='FLQuant',hat='FLQuant'),
    function(obs,hat,rho=0,sigma=NULL){
    ## calculates likelihood for AR(1) process

    if (is.null(sigma)) {
       sigma2<-sigma(obs,hat)
       sigma2<-sigma2^2}
    else
       sigma2<-sigma^2

    n        <-length(obs)
    s2       <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
    s1       <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
    sigma2.a <-(1-rho^2)*sigma2
    res      <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    return(res)})
