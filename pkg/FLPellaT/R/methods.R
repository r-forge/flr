# methods - methods for FLPellaT
# FLPellaT/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# $Id: methods.R 202 2009-03-23 19:16:51Z imosqueira $

####  project stock for pop parameters and catch
proj<-function(catch,r,K,b0=1,mpar=2){
    stock <- rep(K*b0, length(catch)+1)
    names(stock) <- c(as.integer(names(catch)),max(as.integer(names(catch)))+1)

    for(y in seq(2, length(stock)))
         stock[y] <- stock[y-1] + r*stock[y-1]*(1-stock[y-1]^(mpar-1)/K) - catch[y-1]

    stock[stock < 0] <- 1e-8

    return(stock)
    }
    
####  Biomass mid year
mnBio<-function(stock){
    return((stock[-length(stock)]+stock[-1])/2)
    }
    
#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(mnBio,index,error="log"){

   if (error=="log")
      q <- sum(mnBio*index, na.rm=T)/sum(mnBio*mnBio, na.rm=T)
   else
      q <- exp(sum(log(index)-log(mnBio), na.rm=T)/(sum(ifelse(is.na(c(index)),1,0))))

   return(q)
   }

#### Calculate expected catch for given parameters and stock
catchHat<-function(stock,r,K,mpar=2){
   res<-r*stock*(1-stock^(mpar-1)/K)

   return(res)
   }

sigma<-function(index,index_hat,error="log"){

   if (error=="log"){
      SS   <-sum((log(index)-log(index_hat))^2,na.rm=T)}
   else{
      SS   <-sum((index-index_hat)^2,na.rm=T)}

   return((SS/length(index_hat))^.5)
   }

#### calc LL for Schaefer by default
LL <- function(stock,index,params,b0=1.0,mpar=2,error="log",type=1)
   {
   logl<-function(sigma2,obs,hat)
      {
      SS<-sum((obs-hat)^2)
      
      n   <-length(obs)
      res <-(log(1/(2*pi))-n*log(sigma2)-SS/(2*sigma2))/2

      return(res)
      }

   r <- params[1]
   K <- params[2]

   index_hat<-indexHat(stock[names(index)],index,r,K,b0,mpar,error)

   if (error=="log"){
      index    =log(index)
      index_hat=log(index_hat)}
      

   se<-sigma(index,index_hat,error=error)

   if (type==1) return(logl(se,index,index_hat)) else
   if (type==2) return(-sum(dnorm(index, index_hat, se, log=(error=="log")), na.rm=TRUE)) else
   if (type==3) return(sum((index-index_hat)^2))
   }

setGeneric('fit', function(object,...)
		standardGeneric('fit'))

setMethod('fit', signature(object='FLPellaT'),
  function(object,fix=NULL,start=NULL){

     niters<-dim(index(object))[6]
     object@hessian<-array(NA,c(2,2,niters),list(c("r","K"),c("r","K"),iter=1:niters))

     for (i in 1:niters){
         catch.<-c(catch(object)@.Data[1,,,,,i,drop=T])
         index.<-c(index(object)@.Data[1,,,,,i,drop=T])
#         index.[dimnames(catch(object))$year %in% dimnames(index(object))$year]<-c(index(object)@.Data[1,dimnames(index(object))$year %in% dimnames(catch(object))$year,,,,i,drop=T])

      object@params["r",   i]<-0.3
      object@params["K",   i]<-mean(catch.)*1000
      object@params["b0",  i]<-1
      object@params["mpar",i]<-2

      if (!is.null(start))
         object@params[names(start), i]<-start

         par<-c(object@params[c("r","K"),i])
         names(par)<-c("r","K")
         par<-par[!(c("r","K") %in% names(fix))]
         fix<-fix[names(fix) %in% c("r","K")]

         b0  <-c(object@params["b0",  i])
         mpar<-c(object@params["mpar",i])
         res<-nls.lm(par,residuals,fix=fix,b0=b0,mpar=mpar,catch=catch.,index=index.,error=object@distribution)

         for (j in 1:length(res$par))
            object@params[names(par)[j],i]<-res$par[j]

         if (!is.null(fix))
            for (j in 1:length(fix))
               object@params[names(fix)[j],i]<-fix[j]

         r           <-c(object@params["r",i])
         K           <-c(object@params["K",i])
         stock.      <-proj(catch.,r,K,b0,mpar)
         object@stock<-FLQuant(stock.[-length(stock.)], dimnames=dimnames(object@catch))
         indexHat.   <-indexHat(stock.,index.,r,K,b0,mpar,error="log")

         object@params["sigma",i]<-sigma(index.,indexHat.,error="log")
         object@params["q",    i]<-calcQ(mnBio(stock.),index.,error="log")

         object@LL["deviance"]<- res$deviance #LL(catch.,index.,c(r,K),b0,mpar,error="log")
         object@LL["LL1"]<- LL(stock.,index.,c(r,K),b0,mpar,error="log")
         object@LL["LL2"]<- LL(stock.,index.,c(r,K),b0,mpar,error="log",type=2)
         object@LL["LL3"]<- LL(stock.,index.,c(r,K),b0,mpar,error="log",type=3)

         object@hessian[names(par),names(par),i] <-res$hessian
         }
          
     return(object)
     })
     
### Calculate index hat
indexHat<-function(object,index.,r,K,b0=1,mpar=2,error="log")
   {
   ## calculate q
   bio  <-mnBio(object)
   yrs<-names(index.[!is.na(index.)])

   q    <-calcQ(bio[yrs],index.[yrs],error=error)

   return(q*bio)
   }

### Calculate index hat
setGeneric('fitted', function(object,...)
		standardGeneric('fitted'))

setMethod('fitted', signature(object='FLPellaT'),
  function(object){

   r     <-object@params["r",]
   K     <-object@params["K",]
   b0    <-object@params["b0",]
   mpar  <-object@params["mpar",]

   catch.<-catch(object)[1,,,,,,drop=T]
   index.<-index(object)[1,,,,,,drop=T]

   stock. <- proj(catch.,r,K,b0,mpar)
   index_hat<-indexHat(stock.[names(index.)],index.,r,K,b0,mpar,object@distribution)

   res<-FLQuant(indexHat(c(catch.),c(index.),r,K,b0,mpar,error=object@distribution),dimnames=dimnames(index(object)))

   return(res)
   })
   
#### residuals
setGeneric('residuals', function(object,...)
		standardGeneric('residuals'))

setMethod('residuals', signature(object='numeric'),
   function(object,fix,b0=1,mpar=2.0,error="log",catch=catch,index=index)
   {
   if ("r" %in% names(object)) r<-object["r"] else r<-fix["r"]
   if ("K" %in% names(object)) K<-object["K"] else K<-fix["K"]

   stock <- proj(catch,r,K,b0,mpar)
   index_hat<-indexHat(stock,index,r,K,b0,mpar,error)

   if (error=="log"){
      res<-(c(log(index)-log(index_hat)))}
   else{
      res<-(c(index-index_hat))}

   res[is.na(res)]<-10e6

   return(res)
   })

setMethod('residuals', signature(object='FLPellaT'),
   function(object)
     {
     if (object@distribution=="log")
        res<-(log(object@index)-log(fitted(object)))
     else
        res<-(object@index-fitted(object))
        
     return(res)
     })

#### residuals
setGeneric('msy', function(object,...)
		standardGeneric('msy'))
		
#### Reference points
setMethod('msy', signature(object='FLPellaT'),
   function(object)
     {
      bmsy<-function(K,mpar=2){
          (K/mpar)^(1/(mpar-1))}

      msy<-function(r,K,mpar=2){
         r*bmsy(K,mpar)*(1-bmsy(K,mpar)^(mpar-1)/K)}

      fmsy<-function(r,K,mpar=2){
         msy(r,K,mpar)/bmsy(K,mpar)}

     res<-array(NA,c(3,dims(object)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsy(object@params["r",],object@params["K",],object@params["mpar",])
     res["stock",]  <-bmsy(                   object@params["K",],object@params["mpar",])
     res["catch",]  <-msy( object@params["r",],object@params["K",],object@params["mpar",])
     
     return(res)
     })

#### catchHat
setGeneric('computeCatch', function(object,...)
		standardGeneric('computeCatch'))

setMethod('computeCatch', signature(object='FLPellaT'),
catchHat<-function(object,stock){
   res<-object@params["r",]*stock*(1-stock^(object@params["mpar",]-1)/object@params["K",])

   return(res)
   })
