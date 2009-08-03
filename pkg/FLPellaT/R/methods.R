# methods - methods for FLPellaT
# FLPellaT/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# $Id: methods.R 202 2009-03-23 19:16:51Z imosqueira $

####  Biomass mid year
mnBio<-function(x) (x[-length(x)]+x[-1])/2

#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(bio,index,error="log"){
   ####  Biomass mid year
   bio<-mnBio(bio)

   yrs<-names(index)[names(index) %in% names(bio)]

   if (error=="log")
      q <- sum(bio[yrs]*index[yrs], na.rm=T)/sum(bio[yrs]*bio[yrs], na.rm=T)
   else
      q <- exp(sum(log(index[yrs])-log(bio[yrs]), na.rm=T)/(sum(ifelse(is.na(c(index[yrs])),1,0))))

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
LL <- function(obj,hat=NULL,error="log",type=1)
   {
   ## assumes obj are residuals
   if (is.null(hat)){
      hat<-obj
      hat[]<-0}
      
   logl<-function(sigma2,obs,hat)
      {
      SS<-sum((obs-hat)^2)
      
      n   <-length(obs)
      res <-(log(1/(2*pi))-n*log(sigma2)-SS/(2*sigma2))/2

      return(res)
      }

   se<-sigma(obs,hat,error=error)

   if (type==1) return(logl(se,index,index_hat)) else
   if (type==2) return(-sum(dnorm(index, index_hat, se, log=(error=="log")), na.rm=TRUE)) else
   if (type==3) return(sum((index-index_hat)^2))
   }

setGeneric('fit', function(obj,...)
		standardGeneric('fit'))

setMethod('fit', signature(obj='FLPellaT'),
  function(obj,fix=NULL,start=NULL){

     #### set up object to return
     niters<-dim(index(obj))[6]
     parNms<-c("r","K","mpar","b0","q","sigma")
     
     obj@params      <-FLPar(c(.3,NA,2,1,NA,NA),dimnames=list(paramss=parNms,iter=1:niters))
     obj@params@.Data["K",1]<-apply(catch(obj)@.Data,1,mean)*100

     obj@covar   <-array(NA,c(length(parNms),length(parNms),niters),dimnames=list(paramss=parNms,paramss=parNms,iter=1:niters))
     obj@LL      <-numeric(niters)
     obj@sigma   <-numeric(niters)
     obj@deviance<-numeric(niters)
     obj@df      <-array(NA,c(2,niters),dimnames=list(NULL,iter=1:niters))
     obj@stats   <-array(NA,c(length(parNms),3,niters),dimnames=list(paramss=parNms,stats=c("Std. Error","t value","Pr(>|t|)"),iter=1:niters))
     obj@message <-vector(niters,mode="character")

     for (i in 1:niters){

         catch.<-c(catch(obj)@.Data[1,,,,,i,drop=T])
         index.<-c(index(obj)@.Data[1,,,,,i,drop=T])

         if (!is.null(start))
            obj@params[names(start), i]<-start

            par       <-c(obj@params[c("r","K"),i])
            names(par)<-c("r","K")
            par       <-par[!(c("r","K") %in% names(fix))]
            fix       <-fix[ names(fix)  %in% c("r","K") ]

            b0        <-c(obj@params["b0",  i])
            mpar      <-c(obj@params["mpar",i])

            nls.out<-nls.lm(par,residuals,fix=fix,b0=b0,mpar=mpar,catch=catch.,index=index.,error=obj@distribution)

            obj    <-getNLS(obj,nls.out,i)

            if (!is.null(fix))
               for (j in 1:length(fix))
                  obj@params[names(fix)[j],i]<-fix[j]

            r           <-c(obj@params["r",i])
            K           <-c(obj@params["K",i])
            stock.      <-proj(catch.,r,K,b0,mpar)
            obj@stock<-FLQuant(stock.[-length(stock.)], dimnames=dimnames(obj@catch))
            obj@params@.Data["q",i]<-calcQ(stock.,index.,error=obj@distribution)
            }
          
     return(obj)
     })
     
### Calculate index hat
indexHat<-function(obj,q)
   {
   ## calculate q
   bio  <-mnBio(obj)

   return(q*bio)
   }

### Calculate index hat
setGeneric('fitted', function(obj,...)
		standardGeneric('fitted'))

setMethod('fitted', signature(obj='FLPellaT'),
  function(obj){

   r     <-obj@params["r",]
   K     <-obj@params["K",]
   b0    <-obj@params["b0",]
   mpar  <-obj@params["mpar",]

   catch.<-catch(obj)[1,,,,,,drop=T]
   index.<-index(obj)[1,,,,,,drop=T]

   stock. <- proj(catch.,r,K,b0,mpar)
   index_hat<-indexHat(stock.,obj@params["q",])

   res<-FLQuant(index_hat,dimnames=dimnames(index(obj)))

   return(res)
   })
   
#### residuals
setGeneric('residuals', function(obj,...)
		standardGeneric('residuals'))

setMethod('residuals', signature(obj='numeric'),
   function(obj,fix,b0=1,mpar=2.0,error="log",catch=catch,index=index)
   {
   if ("r" %in% names(obj)) r<-obj["r"] else r<-fix["r"]
   if ("K" %in% names(obj)) K<-obj["K"] else K<-fix["K"]

   stock <- proj(catch,r,K,b0,mpar)
   q     <-calcQ(stock,index,error=error)

   index_hat<-indexHat(stock,q)

   if (error=="log"){
      res<-(c(log(index)-log(index_hat)))}
   else{
      res<-(c(index-index_hat))}

   res[is.na(res)]<-10e6

   return(res)
   })

setMethod('residuals', signature(obj='FLPellaT'),
   function(obj)
     {
     if (obj@distribution=="log")
        res<-(log(obj@index)-log(fitted(obj)))
     else
        res<-(obj@index-fitted(obj))

     return(res)
     })

#### residuals
setGeneric('msy', function(obj,...)
		standardGeneric('msy'))
		
#### Reference points
setMethod('msy', signature(obj='FLPellaT'),
   function(obj)
     {
     res<-array(NA,c(3,dims(obj)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsy(obj@params["r",],obj@params["K",],obj@params["mpar",])
     res["stock",]  <-bmsy(                    obj@params["K",],obj@params["mpar",])
     res["catch",]  <-msy.( obj@params["r",],obj@params["K",],obj@params["mpar",])
     
     return(res)
     })

#### catchHat
setGeneric('computeCatch', function(obj,...)
		standardGeneric('computeCatch'))

setMethod('computeCatch', signature(obj='FLPellaT'),
catchHat<-function(obj,stock){
   res<-obj@params["r",]*stock*(1-stock^(obj@params["mpar",]-1)/obj@params["K",])

   return(res)
   })

#### Reference points
setGeneric('msySE', function(obj,...)
		standardGeneric('msySE'))

setMethod('msySE', signature(obj='FLPellaT'),
   function(obj)
     {
     rSE<-0
     kSE<-0
     mSE<-0

     if ("r" %in% dimnames(summary(obj@nlsSmry)$coefficients)[[1]])
        rSE<-(summary(obj@nlsSmry))$coefficients["r","Std. Error"]
     if ("K" %in% dimnames(summary(obj@nlsSmry)$coefficients)[[1]])
        kSE<-(summary(obj@nlsSmry))$coefficients["K","Std. Error"]
     if ("mpa" %in% dimnames(summary(obj@nlsSmry)$coefficients)[[1]])
        mSE<-(summary(obj@nlsSmry))$coefficients["mpar","Std. Error"]

     #### bmsy
     bmsySE<-function(K,mpar=2,kSE=0,mSE=0){
     
          res<-(dBdK(K,mpar)*kSE)^2+
               (dBdM(K,mpar)*mSE)^2

         return(res^0.5)
         }

     #### msy
     msySE.<-function(r,K,mpar=2,rSE=0,kSE=0,mSE=0){

          res<-(dMdK(r,K,mpar)*kSE)^2+
               (dMdR(r,K,mpar)*rSE)^2+
               (dMdM(r,K,mpar)*mSE)^2

         return(res^0.5)
         }

      fmsySE<-function(r,K,mpar=2,rSE=0,kSE=0,mSE=0){

          res<-(dFdK(r,K,mpar)*kSE)^2+
               (dFdR(r,K,mpar)*rSE)^2+
               (dFdM(r,K,mpar)*mSE)^2

         return(res^0.5)
         }

     res<-array(NA,c(3,dims(obj)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsySE(obj@params["r",],obj@params["K",],obj@params["mpar",],rSE,kSE,mSE)
     res["stock",]  <-bmsySE(                    obj@params["K",],obj@params["mpar",],    kSE,mSE)
     res["catch",]  <-msySE.(obj@params["r",],obj@params["K",],obj@params["mpar",],rSE,kSE,mSE)

     return(res)
     })

       bmsy<-function(K,mpar=2){
           (K/mpar)^(1/(mpar-1))}

       msy.<-function(r,K,mpar=2){
          r*bmsy(K,mpar)*(1-bmsy(K,mpar)^(mpar-1)/K)}

       fmsy<-function(r,K,mpar=2){
          r*(1-bmsy(K,mpar)^(mpar-1)/K)}


          dBdK<-function(K,mpar){
            .expr1 <- K/mpar
            .expr3 <- 1/(mpar - 1)
            .value <- .expr1^.expr3
            .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
            .grad[, "K"] <- .expr1^(.expr3 - 1) * (.expr3 * (1/mpar))

            return(.grad)
            }

          dBdM<-function(K,mpar){
            .expr1 <- K/mpar
            .expr2 <- mpar - 1
            .expr3 <- 1/.expr2
            .expr4 <- .expr1^.expr3
            .value <- .expr4
            .grad <- array(0, c(length(.value), 1L), list(NULL, c("mpar")))
            .grad[, "mpar"] <- -(.expr4 * (log(.expr1) * (1/.expr2^2)) +
                .expr1^(.expr3 - 1) * (.expr3 * (K/mpar^2)))

             return(.grad)}


          #deriv(~r*(K/mpar)^(1/(mpar-1))*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"r")
          dMdR<-function(r,K,mpar){
              .expr1 <- K/mpar
              .expr2 <- mpar - 1
              .expr3 <- 1/.expr2
              .expr4 <- .expr1^.expr3
              .expr9 <- 1 - .expr1^.expr3^.expr2/K
              .value <- r * .expr4 * .expr9
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("r")))
              .grad[, "r"] <- .expr4 * .expr9

              return(.grad)}

          #deriv(~r*(K/mpar)^(1/(mpar-1))*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"K")
          dMdK<-function(r,K,mpar){
              .expr1 <- K/mpar
              .expr2 <- mpar - 1
              .expr3 <- 1/.expr2
              .expr5 <- r * .expr1^.expr3
              .expr6 <- .expr3^.expr2
              .expr7 <- .expr1^.expr6
              .expr9 <- 1 - .expr7/K
              .expr13 <- 1/mpar
              .value <- .expr5 * .expr9
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
              .grad[, "K"] <- r * (.expr1^(.expr3 - 1) * (.expr3 * .expr13)) *
                  .expr9 - .expr5 * (.expr1^(.expr6 - 1) * (.expr6 * .expr13)/K -
                  .expr7/K^2)

              return(.grad)}


          #deriv(~r*(K/mpar)^(1/(mpar-1))*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"mpar")
          dMdM<-function(r,K,mpar){
              .expr1 <- K/mpar
              .expr2 <- mpar - 1
              .expr3 <- 1/.expr2
              .expr4 <- .expr1^.expr3
              .expr5 <- r * .expr4
              .expr6 <- .expr3^.expr2
              .expr7 <- .expr1^.expr6
              .expr9 <- 1 - .expr7/K
              .expr11 <- log(.expr1)
              .expr17 <- 1/.expr2^2
              .expr26 <- K/mpar^2
              .value <- .expr5 * .expr9
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("mpar")))
              .grad[, "mpar"] <- -(.expr5 * ((.expr7 * (.expr11 * (.expr6 *
                  log(.expr3) - .expr3^(.expr2 - 1) * (.expr2 * .expr17))) -
                  .expr1^(.expr6 - 1) * (.expr6 * .expr26))/K) + r * (.expr4 *
                  (.expr11 * .expr17) + .expr1^(.expr3 - 1) * (.expr3 *
                  .expr26)) * .expr9)

              return(.grad)}

          #deriv(~r*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"r")
          dFdR<-function(r,K,mpar){
              .expr2 <- mpar - 1
              .expr7 <- 1 - (K/mpar)^(1/.expr2)^.expr2/K
              .value <- r * .expr7
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("r")))
              .grad[, "r"] <- .expr7

              return(.grad)
              }

          #deriv(~r*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"K")
          dFdK<-function(r,K,mpar){
              .expr1 <- K/mpar
              .expr2 <- mpar - 1
              .expr4 <- (1/.expr2)^.expr2
              .expr5 <- .expr1^.expr4
              .value <- r * (1 - .expr5/K)
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
              .grad[, "K"] <- -(r * (.expr1^(.expr4 - 1) * (.expr4 * (1/mpar))/K -
                  .expr5/K^2))

              return(.grad)
              }

          #deriv(~r*(1-(K/mpar)^(1/(mpar-1))^(mpar-1)/K),"mpar")
          dFdM<-function(r,K,mpar){
               .expr1 <- K/mpar
              .expr2 <- mpar - 1
              .expr3 <- 1/.expr2
              .expr4 <- .expr3^.expr2
              .expr5 <- .expr1^.expr4
              .value <- r * (1 - .expr5/K)
              .grad <- array(0, c(length(.value), 1L), list(NULL, c("mpar")))
              .grad[, "mpar"] <- -(r * ((.expr5 * (log(.expr1) * (.expr4 *
                  log(.expr3) - .expr3^(.expr2 - 1) * (.expr2 * (1/.expr2^2)))) -
                  .expr1^(.expr4 - 1) * (.expr4 * (K/mpar^2)))/K))

              return(.grad)
              }

getNLS<-function(obj,nls.out,iter){
    parNms<-dimnames(summary(nls.out)$coefficients)[[1]]
    
    obj@params@.Data[parNms, iter] <- summary(nls.out)$coefficients[,"Estimate"]
    obj@covar[parNms,parNms, iter] <-(summary(nls.out)$cov.unscaled*summary(nls.out)$sigma^2)^.5
#    obj@LL[                 iter] <-LL(nls.out$fvec,error=obj@distribution)
    obj@sigma[               iter] <-summary(nls.out)$sigma
    obj@deviance[            iter] <-nls.out$deviance
    obj@df[,                 iter] <-summary(nls.out)$df
    obj@stats[parNms,,       iter] <-summary(nls.out)$coefficients[,c("Std. Error","t value","Pr(>|t|)")]
    obj@message[             iter] <-nls.out$message

    return(obj)
	  }


####  project stock for pop parameters and catch
setGeneric('proj', function(obj,...)
		standardGeneric('proj'))

setMethod('proj', signature(obj="numeric"),
   function(obj,r,K,b0=1,mpar=2){
    stock <- rep(K*b0, length(obj)+1)
    names(stock) <- c(as.integer(names(obj)),max(as.integer(names(obj)))+1)

    for(y in seq(2, length(stock)))
         stock[y] <- stock[y-1] + r*stock[y-1]*(1-(stock[y-1]^(mpar-1))/K) - obj[y-1]

    stock[stock < 0] <- 1e-8

    return(stock)
    })

setMethod('proj', signature(obj='FLPellaT'),
   function(obj){

    stock <-FLQuant(c(obj@params["K",]*obj@params["b0",]), dimnames=dimnames(catch(object)))
    stock <-window(stock,end=dims(catch(flpt))$maxyear+1)

    for(y in 2:dims(stock)$year) {
         t1       <-sweep(stock[,y-1],6,obj@params["r",],"*")
         t2       <-1-sweep(sweep(stock[,y-1],6,obj@params["mpar",]-1,"^"),6,obj@params["K",],"/")
         stock[,y]<-stock[,y-1]-catch(obj)[,y-1]+t1*t2}
         
    stock[stock < 0] <- 1e-8

    return(stock)
    })

