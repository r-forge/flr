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

calcSigma<-function(index,index_hat,error="log"){
#   if (error=="log"){
#      SS   <-sum((log(index)-log(index_hat))^2,na.rm=T)}
#   else{
      SS   <-sum((index-index_hat)^2,na.rm=T)

   return((SS/length(index_hat))^.5)
   }

#### calc LL for Schaefer by default
getLL<-function(obs,hat,error="log",type=1)
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

setGeneric('fit', function(object,...)
		standardGeneric('fit'))

setMethod('fit', signature(object='FLPellaT'),
  function(object,fix=NULL,start=NULL,concLL=NULL){
  
     #### set up object to return
     niters<-dim(index(object))[6]
     parNms<-c("r","K","mpar","b0","q","sigma")
     object@params             <-FLPar(c(.3,NA,2,1,NA,NA),dimnames=list(paramss=parNms,iter=1:niters))
     object@params@.Data["K",1]<-apply(catch(object)@.Data,1,mean)*100

     object@covar   <-array(NA,c(length(parNms),length(parNms),niters),dimnames=list(paramss=parNms,paramss=parNms,iter=1:niters))
     object@LL      <-numeric(niters)
     object@sigma   <-numeric(niters)
     object@resDev  <-numeric(niters)
     object@dof     <-array(NA,c(2,niters),dimnames=list(NULL,iter=1:niters))
     object@stats   <-array(NA,c(length(parNms),3,niters),dimnames=list(paramss=parNms,stats=c("Std. Error","t value","Pr(>|t|)"),iter=1:niters))
     object@stopmess<-vector(niters,mode="character")

     for (i in 1:niters){
         catch.<-c(catch(object)@.Data[1,,,,,i,drop=T])
         index.<-c(index(object)@.Data[1,,,,,i,drop=T])

         if (!is.null(start))
            object@params@.Data[names(start), i]<-start

            par       <-c(object@params[c("r","K"),i])
            names(par)<-c("r","K")
            par       <-par[!(c("r","K") %in% names(fix))]
            fix       <-fix[ names(fix)  %in% c("r","K") ]

            b0        <-c(object@params["b0",  i])
            mpar      <-c(object@params["mpar",i])

            nls.out<-nls.lm(par,residuals,fix=fix,b0=b0,mpar=mpar,catch=catch.,index=index.,error=object@distribution)

            object    <-getNLS(object,nls.out,i)

            if (!is.null(fix))
               for (j in 1:length(fix))
                  object@params@.Data[names(fix)[j],i]<-fix[j]

            r           <-c(object@params["r",i])
            K           <-c(object@params["K",i])
            stock.      <-proj(catch.,r,K,b0,mpar)
            object@stock<-FLQuant(stock.[-length(stock.)], dimnames=dimnames(object@catch))
            object@params@.Data["q",i]<-calcQ(stock.,index.,error=object@distribution)
            }
          
     return(object)
     })
     
### Calculate index hat
indexHat<-function(object,q)
   {
   ## calculate q
   bio  <-mnBio(object)

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
   index_hat<-indexHat(stock.,object@params["q",])

   res<-FLQuant(index_hat,dimnames=dimnames(index(object)))

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

   stock <-proj(catch[names(index)],r,K,b0,mpar)
   q     <-calcQ(stock,index,error=error)

   index_hat<-indexHat(stock,q)

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
     res<-array(NA,c(3,dims(object)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsy(object@params["r",],object@params["K",],object@params["mpar",])
     res["stock",]  <-bmsy(                    object@params["K",],object@params["mpar",])
     res["catch",]  <-msy.( object@params["r",],object@params["K",],object@params["mpar",])
     
     return(res)
     })

#### catchHat
setMethod('computeCatch', signature(object='FLPellaT'),
catchHat<-function(object,stock){
   res<-object@params["r",]*stock*(1-stock^(object@params["mpar",]-1)/object@params["K",])

   return(res)
   })

#### Reference points
setGeneric('msySE', function(object,...)
		standardGeneric('msySE'))

setMethod('msySE', signature(object='FLPellaT'),
   function(object)
     {
     #### bmsy
     bmsySE<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dBdK(K,mpar)^2*cvr["K",   "K",   ]+
               dBdM(K,mpar)^2*cvr["mpar","mpar",]+

               dBdK(K,mpar)*dBdM(K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     #### msy
     msySE.<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dMdR(r,K,mpar)^2*cvr["r",   "r",   ]+
               dMdK(r,K,mpar)^2*cvr["K",   "K",   ]+
               dMdM(r,K,mpar)^2*cvr["mpar","mpar",]+

               dMdR(r,K,mpar)*dMdK(r,K,mpar)*cvr["r","K",   ]+
               dMdR(r,K,mpar)*dMdM(r,K,mpar)*cvr["r","mpar",]+
               dMdK(r,K,mpar)*dMdM(r,K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     #### fmsy
     fmsySE<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dFdR(r,K,mpar)^2*cvr["r",   "r",   ]+
               dFdK(r,K,mpar)^2*cvr["K",   "K",   ]+
               dFdM(r,K,mpar)^2*cvr["mpar","mpar",]+
               
               dFdR(r,K,mpar)*dFdK(r,K,mpar)*cvr["r","K",   ]+
               dFdR(r,K,mpar)*dFdM(r,K,mpar)*cvr["r","mpar",]+
               dFdK(r,K,mpar)*dFdM(r,K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     res            <-array(NA,c(3,dims(object)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsySE(object)
     res["stock",]  <-bmsySE(object)
     res["catch",]  <-msySE.(object)

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

getNLS<-function(object,nls.out,iter){
    parNms<-dimnames(summary(nls.out)$coefficients)[[1]]

    object@params@.Data[parNms, iter] <- summary(nls.out)$coefficients[,"Estimate"]
    object@covar[parNms,parNms, iter] <-(summary(nls.out)$cov.unscaled*summary(nls.out)$sigma^2)
    object@LL[                  iter] <-getLL(nls.out$fvec,rep(ifelse(object@distribution=="log",1,0),length(nls.out$fvec)),error=object@distribution)
    object@sigma[               iter] <-summary(nls.out)$sigma
    object@resDev[              iter] <-nls.out$deviance
    object@dof[,                iter] <-summary(nls.out)$df
    object@stats[parNms,,       iter] <-summary(nls.out)$coefficients[,c("Std. Error","t value","Pr(>|t|)")]
    object@stopmess[            iter] <-nls.out$message

    return(object)
	  }

setGeneric('proj', function(object,...)
		standardGeneric('proj'))

####  project stock for pop parameters and catch
setMethod('proj', signature(object="numeric"),
   function(object,r,K,b0=1,mpar=2){
    stock <- rep(K*b0, length(object)+1)

    names(stock) <- c(as.integer(names(object)),max(as.integer(names(object)))+1)

    for(y in seq(2, length(stock)))
         stock[y] <- stock[y-1] + r*stock[y-1]*(1-(stock[y-1]^(mpar-1))/K) - object[y-1]

    stock[stock < 0] <- 1e-8

    return(stock)
    })

setMethod('proj', signature(object='FLPellaT'),
   function(object){

    stock <-FLQuant(c(object@params["K",]*object@params["b0",]), dimnames=dimnames(catch(object)))
    stock <-window(stock,end=dims(catch(flpt))$maxyear+1)

    for(y in 2:dims(stock)$year) {
         t1       <-sweep(stock[,y-1],6,object@params["r",],"*")
         t2       <-1-sweep(sweep(stock[,y-1],6,object@params["mpar",]-1,"^"),6,object@params["K",],"/")
         stock[,y]<-stock[,y-1]-catch(object)[,y-1]+t1*t2}
         
    stock[stock < 0] <- 1e-8

    return(stock)
    })

