# methods - methods for FLPellaT
# FLPellaT/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# $Id: methods.R 202 2009-03-23 19:16:51Z imosqueira $

####  project stock for pop parameters and catch
proj<-function(catch,r,K,b0=1,mpar=2){
    stock <- c(rep(K*b0, length(catch)+1))
    catch <- c(catch)

    for(y in seq(2, length(stock)))
         stock[y] <- stock[y-1] + r*stock[y-1]*(1-stock[y-1]^(mpar-1)/K) - catch[y-1]

    stock[stock < 0] <- 1e-10

    return(stock)
    }
    
####  Biomass mid year
mnBio<-function(stock){
    return((stock[-length(stock)]+stock[-1])/2)
    }
    
#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(mnBio,index,error="log"){

   if (error=="log")
      q <- sum(mnBio*index)/sum(mnBio*mnBio)
   else
      q <- exp(sum(log(index)-log(mnBio))/(length(index)))

   return(q)
   }

#### Calculate expected catch for given parameters and stock
catchHat<-function(stock,r,K,mpar=2){
   res<-r*stock*(1-stock^(mpar-1)/K)

   return(res)
   }

sigma<-function(index,index_hat,error="log"){
   yrs<-dimnames(index)$year[dimnames(index)$year %in%  dimnames(index)$year]

   if (error=="log")
      SS   <-sum((log(index[yrs])-log(index_hat))^2)
   else
      SS   <-sum((index[yrs]-index_hat)^2)

   return((SS/length(index_hat))^.5)
   }


#### calc LL for Schaefer by default
LL <- function(catch,index,params,b0=1.0,mpar=2,error="log")
   {
   r <- params[1]
   K <- params[2]

   index_hat<-indexHat(catch,index,r,K,b0,mpar,error)

   if (error=="log")
      index=log(index)

   -sum(dnorm(index, index_hat, sigma(index,index_hat,error=error),
                log=(error=="log")), na.rm=TRUE)
   }
   
setGeneric('fit', function(object,...)
		standardGeneric('fit'))

setMethod('fit', signature(object='FLPellaT'),
  function(object){

     niters<-dim(index(object))[6]
     object@hessian<-array(NA,c(2,2,niters),list(c("r","K"),c("r","K"),       iter=1:niters))

     for (i in 1:niters){

         catch.<-c(catch(object)@.Data[1,,,,,i,drop=T])
         index.<-c(index(object)@.Data[1,,,,,i,drop=T])

         if (is.na(object@param["K",   i])) object@param["K",   i]   <-mean(catch.)*10
         if (is.na(object@param["r",   i])) object@param["r",   i]   <-0.3
         if (is.na(object@param["b0",  i])) object@param["b0",  i]  <-1.0
         if (is.na(object@param["mpar",i])) object@param["mpar",i]<-2.0

         b0   <-c(object@param["b0",  i])
         mpar <-c(object@param["mpar",i])

         res<-nls.lm(c(object@param[c("r","K"),i]),residuals,b0=b0,mpar=mpar,catch=catch.,index=index.,error=object@distribution)

         object@param["r",i]<-res$par[1]
         object@param["K",i]<-res$par[2]

         r     <-c(object@param["r",   i])
         K     <-c(object@param["K",   i])
         stock.<-proj(catch.,c(object@param["r",i]),c(object@param["K",i]),c(object@param["b0",i]),c(object@param["mpar",i]))

         object@hessian[,,i]<-res$hessian
         object@stock       <-FLQuant(stock.)
         indexHat.          <-indexHat(catch.,index.,r,K,b0,mpar,error="log")

         object@param["sigma",i]<-sigma(index.,indexHat.,error="log")
         object@param["q",    i]<-calcQ(mnBio(stock.),index.,error="log")
         LL(object)              <-LL(catch.,index.,c(r,K),b0,mpar,error="log")
         }
          
     return(object)
     })
     
### Calculate index hat
indexHat<-function(object,index,r,K,b0=1,mpar=2,error="log")
   {
   stock <- proj(object,r,K,b0,mpar)

   ## calculate q
   bio  <-mnBio(stock)
   q    <-calcQ(bio,index,error=error)

   return(q*bio)
   }

### Calculate index hat
setGeneric('fitted', function(object,...)
		standardGeneric('fitted'))

setMethod('fitted', signature(object='FLPellaT'),
  function(object){
  
   catch.<-catch(object)
   index.<-index(object)
   r     <-object@param["r",]
   K     <-object@param["K",]
   b0    <-object@param["b0",]
   mpar  <-object@param["mpar",]
   
   res<-FLQuant(indexHat(c(catch.),c(index.),r,K,b0,mpar,error=object@distribution),dimnames=dimnames(index(object)))

   return(res)
   })
   
#### residuals
setGeneric('residuals', function(object,...)
		standardGeneric('residuals'))

setMethod('residuals', signature(object='numeric'),
   function(object,b0=1.0,mpar=2.0,error="LOG",catch=catch,index=index)
   {
   r <- object[1]
   K <- object[2]

   index_hat<-indexHat(catch,index,r,K,b0,mpar,error)

   if (error=="LOG")
      return(c(log(index)-log(index_hat)))
   else
      return(c(index-index_hat))
   })

setMethod('residuals', signature(object='FLPellaT'),
   function(object)
     {
     if (object@distribution=="log")
        return(log(object@index)-log(fitted(object)))
     else
        return(object@index-fitted(object))
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
     res["harvest",]<-fmsy(object@param["r",],object@param["K",],object@param["mpar",])
     res["stock",]  <-bmsy(                   object@param["K",],object@param["mpar",])
     res["catch",]  <-msy( object@param["r",],object@param["K",],object@param["mpar",])
     
     return(res)
     })

#### catchHat
setGeneric('computeCatch', function(object,...)
		standardGeneric('computeCatch'))

setMethod('computeCatch', signature(object='FLPellaT'),
catchHat<-function(object,stock){
   res<-object@param["r",]*stock*(1-stock^(object@param["mpar",]-1)/object@param["K",])

   return(res)
   })
