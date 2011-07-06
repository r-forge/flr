# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# mnBio {{{
mnBio<-function(x) (x[,-dim(x)[2],,,,,drop=FALSE]+x[,-1,,,,,drop=FALSE])/2
# }}}

# calcQ {{{
calcQ<-function(bio,idx,error="log"){
  ####  Biomass mid year
  bio<-mnBio(bio)
  yrs<-dimnames(idx)$year[dimnames(idx)$year %in% dimnames(bio)$year]

  bio<-bio[,yrs,,,,,drop=FALSE]
  idx<-idx[,yrs,,,,,drop=FALSE]

  if (error=="log"){
    q <- sum(bio*idx, na.rm=T)/sum(bio*bio, na.rm=T)}
  else if (error=="normal"){
    q <- exp(sum(log(idx)-log(bio), na.rm=T)/(sum(ifelse(is.na(c(idx)),1,0))))}
  else if (error=="cv"){
    res   <-sum(idx/bio)
    sigma2<-calcSigma(res)
    q     <-(-res+(res^2+4*length(idx)*sigma2*sum((idx/bio)^2)))/(2*length(idx)*sigma2)
  }
  return(q)
}
# }}}

# catchHat
catchHat<-function(stock,r,K,p=2){
   res<-r*stock*(1-stock^(p-1)/K)

   return(res)
   }

# calcSigma
calcSigma<-function(obs,hat=rep(0,length(obs)),error="log"){
      SS   <-sum((obs-hat)^2,na.rm=T)

   return((SS/length(hat))^.5)
   }

# calcLogLik
calcLogLik<-function(obs,hat=rep(0,length(obs)),error="log",type=1)
   {
   logl<-function(se,obs,hat)
      {
      SS<-sum((obs-hat)^2)
      
      n   <-length(obs)
      res <-(log(1/(2*pi))-n*log(se)-SS/(2*se^2))/2

      return(res)
      }

   se<-calcSigma(obs,hat,error=error)

   if (type==1) return(logl(se,obs,hat)) else
   if (type==2) return(-sum(dnorm(obs, hat, se, log=(error=="log"), na.rm=TRUE))) else
   if (type==3) return(sum((obs-hat)^2))
   }

# calcB0
calcB0<-function(index,params,nyrB0,error="log"){
   if (is.null(nyrB0)) return(params["b0",])


   if (error=="log")
      t.<-sweep(log(index[,1:nyrB0,,,,,drop=FALSE]),c(1,6),params["q",],"/")
   else if (error=="normal")
      t.<-sweep(index[,1:nyrB0,,,,,drop=FALSE],c(1,6),params["q",],"/")

   return(exp(apply(t.,c(1,6),mean))/params["K",])
   }


