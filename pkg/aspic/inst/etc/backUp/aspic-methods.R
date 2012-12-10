setMethod('loglAR1', signature(obs='FLQuant', hat='missing'),
  function(obs, hat.=FLQuant(0,dimnames=dimnames(obs)), rho=0){
    hat=hat.
    # calculates likelihood for AR(1) process
    n   <- dim(obs)[2]
    rsdl<-(obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])
    
    s2  <- sum(rsdl^2, na.rm=T)
    s1  <-s2

    if (!all(is.na(rsdl[,1])))
      s1 <- s1+(1-rho^2)*(obs[,1]-hat[,1])^2

    #if (all(is.na(hat))) sigma2<-1e100 else

    sigma2   <- sigma(obs, hat)^2
    
    n        <- length(obs[!is.na(obs)])
    sigma2.a <- (1-rho^2)*sigma2
    res      <- (log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    if (!is.finite(res))
      res <- -1e100

    return(res)}) 

#loglAR1(obs=FLQuant(rlnorm(100)))
