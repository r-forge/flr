# methods - methods for FLBioDym
# FLBioDym/R/methods.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# $Id: methods.R 202 2009-03-23 19:16:51Z imosqueira $

if (!isGeneric("vb")) {
	setGeneric("vb", function(object, ...){
		value  <-  standardGeneric("vb")
		value
	})}

setGeneric('vb', function(object,...)
		standardGeneric('vb'))

####  Biomass mid year
mnBio<-function(x) (x[,-dim(x)[2],,,,,drop=FALSE]+x[,-1,,,,,drop=FALSE])/2

setParams<-function(model="pellat"){
      res<-switch(model,
           fox     =return(FLPar(NA,dimnames=list(params=c("r",  "K"    ,"b0","q","sigma"),iter=1))),
           schaefer=return(FLPar(NA,dimnames=list(params=c("r",  "K","p","b0","q","sigma"),iter=1))),
           gulland =return(FLPar(NA,dimnames=list(params=c("r",  "K"    ,"b0","q","sigma"),iter=1))),
           fletcher=return(FLPar(NA,dimnames=list(params=c("msy","K","p","b0","q","sigma"),iter=1))),
           pellat  =return(FLPar(NA,dimnames=list(params=c("r",  "K","p","b0","q","sigma"),iter=1))),
           shepherd=return(FLPar(NA,dimnames=list(params=c("r",  "K","m","b0","q","sigma"),iter=1))),
           return(NULL))
   }
   
#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(bio,idx,error="log"){
   ####  Biomass mid year
   bio<-mnBio(bio)
   yrs<-dimnames(idx)$year[dimnames(idx)$year %in% dimnames(bio)$year]

   bio<-bio[,yrs,,,,,drop=FALSE]
   idx<-idx[,yrs,,,,,drop=FALSE]

   if (error=="log")
      q <- sum(bio*idx, na.rm=T)/sum(bio*bio, na.rm=T)
   else
      q <- exp(sum(log(idx)-log(bio), na.rm=T)/(sum(ifelse(is.na(c(idx)),1,0))))

   return(q)
   }

#### Calculate expected catch for given parameters and stock
catchHat<-function(stock,r,K,p=2){
   res<-r*stock*(1-stock^(p-1)/K)

   return(res)
   }

calcSigma<-function(obs,hat=rep(0,length(obs)),error="log"){
#   if (error=="log"){
#      SS   <-sum((log(index)-log(index_hat))^2,na.rm=T)}
#   else{
      SS   <-sum((obs-hat)^2,na.rm=T)

   return((SS/length(hat))^.5)
   }

#### calc logLik for Schaefer by default
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

setGeneric('fit', function(object,...)
		standardGeneric('fit'))

setMethod('fit', signature(object='FLBioDym'),
f.<-  function(object,fix=c(p=2.0,b0=1.0),start=NULL,minimiser="nls.lm"){
     #### set up objects to return
     ## parameters
     niters<-dim(index(object))[6]
     parNms<-c("r","K","p","b0","q","sigma")
     object@params                         <-FLPar(c(.3,NA,2,1,NA,NA),dimnames=list(paramss=parNms,iter=1:niters))
     object@params@.Data["r",]             <-0.3
     object@params@.Data["K",]             <-mean(catch(object))*20
     if (!is.null(start))
        object@params@.Data[names(start),niters]<-start[names(start)]
     object@params@.Data[names(fix),  niters]<-fix[  names(fix)  ]

print(start)
print(object@params)

     ## pocket protector stuff
     object@vcov    <-array(NA,c(length(parNms),length(parNms),niters),dimnames=list(paramss=parNms,paramss=parNms,iter=1:niters))
     object@hessian <-object@vcov
     object@logLik  <-numeric(niters)
     object@rsdlVar <-numeric(niters)
     object@dof     <-array(NA,c(2,niters),dimnames=list(NULL,iter=1:niters))
     object@stats   <-array(NA,c(length(parNms),3,niters),dimnames=list(paramss=parNms,stats=c("Std. Error","t value","Pr(>|t|)"),iter=1:niters))
     object@stopmess<-vector(niters,mode="character")
     object@stock   <-object@catch

     if (dims(object@catch)$iter!=niters)
         object@stock   <-propagate(object@stock,niters)

     for (i in 1:niters){
         stock.<-iter(stock(object),i)@.Data
         catch.<-iter(catch(object),i)@.Data
         index.<-iter(index(object),i)@.Data

         if (!is.null(start))
            object@params@.Data[names(start), i]<-start

            #### normally it is only r & K that you need to estimate
            par       <-c(object@params[c("r","K"),i])
            names(par)<-c("r","K")
            par       <-par[!(c("r","K") %in% names(fix))]
            fix       <-fix[ names(fix)  %in% c("r","K") ]

            b0        <-c(object@params["b0",  i])
            p         <-c(object@params["p",i])

            #### Estimate parameters
            if (is.null(fix) || !all(c("r","K") %in% names(fix))){
                 if (minimiser=="optim"){
                    ctrl=list(trace=10,parscale=c(r=.5,K=mean(catch(object))*10))
                    nls.out<-optim(par=par,fn=LL,fix=fix,b0=b0,p=p,stock=stock.,catch=catch.,index=index.,error=object@distribution,
                                    method = "BFGS",control=ctrl,hessian=TRUE)
                    object <-getOptim(object,catch.,index.,nls.out,i)}
                 else {
                    nls.out<-nls.lm(par=par,fn=rsdl,fix=fix,b0=b0,p=p,stock=stock.,catch=catch.,index=index.,error=object@distribution,control=list(nprint=2))
                    object <-getNLS(object,catch.,index.,nls.out,i)}
                 }
            #### All Parameters fixed
            else {
                 object@params@.Data[names(fix),i]<-fix

                 stock.[]<-b0*fix["K"]
                 stock.                    <-fwdArray(object=stock.,model="pellat",catch=catch.,r=c(object@params["r",i]),K=c(object@params["K",i]),p=c(object@params["p",i]))
                 object@stock              <-FLQuant(stock.[-length(stock.)], dimnames=dimnames(object@catch))
                 object@params@.Data["q",i]<-calcQ(stock.,index.,error=object@distribution)
                 rsdl.                     <-rsdl(object=par,fix=fix,b0=b0,p=p,stock=stock.,catch=catch.,index=index.,error=object@distribution)

                 object@vcov[parNms,parNms,  i]<-NA
                 object@logLik[              i]<-calcLogLik(rsdl.,error=object@distribution)
                 object@params@.Data["sigma",i]<-calcSigma(rsdl.)
                 object@rsdlVar[             i]<-sum(rsdl.^2)
                 object@dof[,                i]<-c(0,length(rsdl))
                 object@stats[parNms,,       i]<-NA
                 object@stopmess[            i]<-"didn´t run, all parameters fixed"
	               }
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

setMethod('fitted', signature(object='FLBioDym'),
  function(object){

   index_hat<-sweep(stock(object),c(6),object@params["q",],"*")

   return(index_hat)
   })
   
#### residuals
setGeneric('residuals', function(object,...)
		standardGeneric('residuals'))

setMethod('residuals', signature(object='array'),
rsdl<-function(object,fix,model="pellat",b0=1,p=2.0,error="log",stock=NULL,catch=NULL,index=NULL)
   {
   if ("r" %in% names(object)) r<-object["r"] else r<-fix["r"]
   if ("K" %in% names(object)) K<-object["K"] else K<-fix["K"]

   stock[]<-b0*K
   stock <-fwdArray(stock,model=model,catch=catch,r=r,K=K,p=p)

   yrs<-dimnames(index)$year[dimnames(index)$year %in% dimnames(stock)$year]

   q     <-calcQ(stock[,yrs,,,,,drop=FALSE],index[,yrs,,,,,drop=FALSE],error=error)

   index_hat<-indexHat(stock,q)
   index_hat<-index_hat[,yrs,,,,,drop=FALSE]
   index    <-index[,yrs,,,,,drop=FALSE]

   if (error=="log"){
      res<-(c(log(index)-log(index_hat)))}
   else{
      res<-(c(index-index_hat))}

   res[is.na(res)]<-1e6
   
   yrs<-!is.na(index[1,,1,1,1,1])

   return(res)
   })

rsdl2<-function(object,fix,flbd)
   {
   if ("r" %in% names(object)) r<-object["r"] else r<-fix["r"]
   if ("K" %in% names(object)) K<-object["K"] else K<-fix["K"]

   stock(flbd)@.Data[]<-b0*K
   stock(flbd)@.Data  <-fwdArray(stock(flbd)@.Data,model=model(flbd),catch=catch(flbd)@.Data,r=r,K=K,p=p)
   yrs<-dimnames(index(flbd)@.Data)$year[dimnames(index(flbd))$year %in% dimnames(stock(flbd))$year]

   q     <-calcQ(stock(flbd)@.Data[,yrs,,,,,drop=FALSE],index[,yrs,,,,,drop=FALSE],error=error)

   index_hat        <-indexHat(stock(flbd)@.Data,q)
   index_hat        <-index_hat[,yrs,,,,,drop=FALSE]
   index(flbd)@.Data<-index(flbd)@.Data[,yrs,,,,,drop=FALSE]

   if (error=="log"){
      res<-(c(log(index(flbd)@.Data)-log(index_hat)))}
   else{
      res<-(c(index(flbd)@.Data-index_hat))}

   res[is.na(res)]<-1e6

   yrs<-!is.na(index[1,,1,1,1,1])

   return(res)
   }

LL<-function(object,fix,model="pellat",b0=1,p=2.0,error="log",stock=NULL,catch=NULL,index=NULL){
    if ("K" %in% names(fix)) K<-fix["K"] else  K<-object["K"]
       stock[]<-b0*K
   -calcLogLik(rsdl(object,fix,model=model,b0=b0,p=p,error=error,stock=stock,catch=catch,index=index),error=error)
   }

setMethod('residuals', signature(object='FLBioDym'),
   function(object)
     {
     if (object@distribution=="log")
        res<-(log(object@index)-log(fitted(object)[,dimnames(object@index)$year]))
     else
        res<-(object@index-fitted(object)[,dimnames(object@index)$year])

     return(res)
     })

getNLS<-function(object,catch.,index.,nls.out,i){
    parNms                        <-dimnames(summary(nls.out)$coefficients)[[1]]
    object@params@.Data[parNms, i]<- summary(nls.out)$coefficients[,"Estimate"]
    object@vcov[parNms,parNms, i] <-(summary(nls.out)$cov.unscaled*summary(nls.out)$sigma^2)
    iter(object@stock,i)[]        <-c(object@params["K",i])*c(object@params["b0",i])

    stock.                        <-fwdArray(object=iter(object@stock,i)@.Data,model="pellat",catch=catch.,r=c(object@params["r",i]),K=c(object@params["K",i]),p=c(object@params["p",i]))
    iter(object@stock,i)          <-FLQuant(stock.[-length(stock.)], dimnames=dimnames(object@catch))
    object@params@.Data["q",i]    <-calcQ(stock.,index.,error=object@distribution)
    object@params@.Data["sigma",i]<-summary(nls.out)$sigma

    object@logLik[              i]<-calcLogLik(nls.out$fvec,rep(0,length(nls.out$fvec)),error=object@distribution)
    object@rsdlVar[             i]<-nls.out$deviance
    object@dof[,                i]<-summary(nls.out)$df
    object@stats[parNms,,       i]<-summary(nls.out)$coefficients[,c("Std. Error","t value","Pr(>|t|)")]
    object@stopmess[            i]<-nls.out$message

    return(object)
	  }

getOptim<-function(object,catch.,index.,nls.out,i){
    parNms                        <-names(nls.out$par)
    object@params@.Data[parNms,   i]<-nls.out$par
    #object@stopmess[              i]<-as.character(nls.out$message)
    object@hessian[parNms,parNms, i]<-nls.out$hessian[parNms,parNms]


    object@vcov[parNms,parNms, i] <--ginv(nls.out$hessian[parNms,parNms])
#    iter(object@stock,i)[]        <-c(object@params["K",i])*c(object@params["b0",i])

    stock.                        <-fwdArray(object=iter(object@stock,i)@.Data,model="pellat",catch=catch.,r=c(object@params["r",i]),K=c(object@params["K",i]),p=c(object@params["p",i]))
    iter(object@stock,i)          <-FLQuant(stock.[-length(stock.)], dimnames=dimnames(object@catch))
    object@params@.Data["q",i]    <-calcQ(stock.,index.,error=object@distribution)
    object@params@.Data["sigma",i]<-calcSigma(residuals(object))

#    object@logLik[              i]<-calcLogLik(nls.out$fvec,rep(0,length(nls.out$fvec)),error=object@distribution)
#    object@rsdlVar[             i]<-nls.out$deviance
#    object@dof[,                i]<-summary(nls.out)$df
#    object@stats[parNms,,       i]<-summary(nls.out)$coefficients[,c("Std. Error","t value","Pr(>|t|)")]

    return(object)
	  }

setMethod('vb', signature(object='FLBioDym'),
   function(object)
      {

      return(object)
      })


#Get the hessian then calculate the eigen values e.g.
#
#eigen(ginv(vcov(flpt)[1:2,1:2,1,drop=T]),symmetric=T)
#
#If the smallest eigenvalue is negative, it says that optim found a saddle point.
#If the smallest eigenvalue is less than ~ 1e-8 times the largest, it says
#   that the smallest eigenvector is very poorly estimated.
# Rounding these off to 1 significant digit will suggest which parameter you can
# fix and drop from the model.


