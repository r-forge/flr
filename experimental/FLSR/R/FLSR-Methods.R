
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
