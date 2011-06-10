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

#### Calculate Q for use in constricted likelihoods etc
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

#### Calculate expected catch for given parameters and stock
catchHat<-function(stock,r,K,p=2){
   res<-r*stock*(1-stock^(p-1)/K)

   return(res)
   }

calcSigma<-function(obs,hat=rep(0,length(obs)),error="log"){
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

calcB0<-function(index,params,nyrB0,error="log"){
   if (is.null(nyrB0)) return(params["b0",])


   if (error=="log")
      t.<-sweep(log(index[,1:nyrB0,,,,,drop=FALSE]),c(1,6),params["q",],"/")
   else if (error=="normal")
      t.<-sweep(index[,1:nyrB0,,,,,drop=FALSE],c(1,6),params["q",],"/")

   return(exp(apply(t.,c(1,6),mean))/params["K",])
   }

setGeneric('fit', function(object,...)
		standardGeneric('fit'))

setMethod('fit', signature(object='FLBioDym'),
f.<-  function(object,fixed=c(b0=1.0,p=2.0,m=0.5),start=NULL,minimiser="nls.lm",model=NULL,nlsControl=nls.lm.control(),nyrB0=NULL){
     if (!is.null(model) & is.character(model))
        model(object)<-model
        
     ## parameters
     object@params<-setParams(model(object),its=dims(object)[6])
     object@params<-defaultPar(object)

     startNms<-names(start)[names(start) %in% dimnames(object@params)$params]
     fixNms  <-names(fixed)[names(fixed) %in% dimnames(object@params)$params]

     if (!is.null(start))
        object@params@.Data[startNms,]<-start[startNms]
     if (!is.null(fixed))
        object@params@.Data[fixNms,  ]<-fixed[fixNms]
        
     parNms<-c(parLst[[model(object)]][1:2],startNms)
     parNms<-unique(parNms[!(parNms %in% fixNms)])

     niters<-dims(object)$iter
     ## pocket protector stuff
     object@vcov    <-array(NA,c(length(parNms),length(parNms),niters),dimnames=list(params=parNms,params=parNms,iter=1:niters))
     object@hessian <-object@vcov
     object@logLik  <-numeric(niters)
     object@rsdlVar <-numeric(niters)
     object@dof     <-array(NA,c(2,niters),dimnames=list(NULL,iter=1:niters))
     object@stats   <-array(NA,c(length(parNms),3,niters),dimnames=list(params=parNms,stats=c("Std. Error","t value","Pr(>|t|)"),iter=1:niters))
     object@stopmess<-vector(niters,mode="character")
     object@stock   <-object@catch

     if (dims(object@catch)$iter!=niters)
         object@stock<-propagate(object@stock,niters)

     for (i in 1:niters){
         ctch<-iter(catch(object),i)@.Data
         indx<-iter(index(object),i)@.Data

         #### Estimate parameters
         if (is.null(fixed) || !all(parLst[[model(object)]] %in% names(fixed))){
            if (minimiser=="optim"){
                 ctrl=list(trace=10,parscale=c(r=.5,K=mean(catch(object))*10))
                 nls.out<-optim(fn=LL,par=params(object)[parNms,i,drop=T],params=params(object)[,i],model=model(object),catch=ctch,index=indx,error=object@distribution,
                                 method = "BFGS",control=ctrl,hessian=TRUE)

                 object <-getOptim(object,ctch,indx,nls.out,i)}
            else {
                 nls.out<-nls.lm(fn=rsdl,par=array(params(object)[parNms,i,drop=T],length(parNms),dimnames=list(parNms)),
                                        params=params(object)[,i],model=model(object),catch=ctch,index=indx,error=object@distribution,nyrB0=nyrB0,control=nlsControl)

                 object <-getNLS(object,ctch,indx,nls.out,i,nyrB0=nyrB0)
                 }
            }
         #### All Parameters fixed
         else {
              object@params@.Data[names(fixed),i]<-fixed
              object@stock[]<-getPar(params(object),"b0")*getPar(params(object),"K")

              stk           <-fwdArray(object=stock(object)@.Data,catch=catch(object)@.Data,model=model(object),
                                                          r  =getPar(params(object)[,i],"r"),
                                                          K  =getPar(params(object)[,i],"K"),
                                                          msy=getPar(params(object)[,i],"msy"),
                                                          p  =getPar(params(object)[,i],"p"))
              object@stock              <-FLQuant(stk[-length(stk)], dimnames=dimnames(object@catch))
              object@params@.Data["q",i]<-calcQ(stk,index(object)@.Data,error=object@distribution)

              rsdl.                     <-rsdl(object=NULL,params=params(object)[,i],model=model(object),catch=ctch,index=indx,error=object@distribution)
              object@params@.Data["sigma", i]<-calcSigma(rsdl.)

#              rsdl(params(object)[parNms,i,drop=T],params=params(object)[,i],model=model(object),stock=stck,catch=ctch,index=indx,error=object@distribution,control=list(nprint=2))
              
              object@vcov[parNms,parNms,  i]<-NA
              object@logLik[              i]<-calcLogLik(rsdl.,error=object@distribution)
#              object@params@.Data["sigma",i]<-calcSigma(rsdl.)
#              object@rsdlVar[             i]<-sum(rsdl.^2)
#              object@dof[,                i]<-c(0,length(rsdl))
              object@stats[parNms,,       i]<-NA
              object@stopmess[            i]<-"didn't run, all parameters fixed"
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
rsdl<-function(object,params,model="pellat",error="log",catch=NULL,index=NULL,nyrB0=3)
   {
   params@.Data["b0",]<-calcB0(index,params,nyrB0)

   if (!is.null(object)) params[names(object),]<-object

   stock<-catch

   stock[]<-getPar(params,"b0")*getPar(params,"K")
   stock <-fwdArray(stock,model=model,catch=catch,r=getPar(params,"r"),K=getPar(params,"K"),p=getPar(params,"p"),m=getPar(params,"m"),msy=getPar(params,"msy"),b0=getPar(params,"b0"))

   yrs<-dimnames(index)$year[dimnames(index)$year %in% dimnames(stock)$year]

   #### Needed in case of jacknife
   yrs<-yrs[!is.na(index[,ac(yrs),,,,])]
#   if ("q" %in% dimnames(object)$params)
       q<-calcQ(stock,index,error=error)
#    else
#       q<-getPar(params,"q")

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

LL<-function(par,model="pellat",error="log",stock=NULL,catch=NULL,index=NULL){
   rs <-rsdl(par,model,error,stock,catch,index)
   rs<- -calcLogLik(rs,error=error)

   return(rs)
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

getNLS<-function(object,catch.,index.,nls.out,i,nyrB0=NULL){
    parNms                        <-dimnames(summary(nls.out)$coefficients)[[1]]
    object@params@.Data[parNms, i]<-summary(nls.out)$coefficients[,"Estimate"]
    object@params@.Data["b0",]    <-calcB0(index.,object@params,nyrB0)
    iter(object@stock,i)[]        <-c(object@params["K",i])*c(object@params["b0",i])

    object                        <-fwd(object)
    object@params@.Data["sigma",i]<-calcSigma(nls.out$fvec)

    yrs<-dimnames(index)$year[dimnames(index)$year %in% dimnames(object@stock)$year]

    object@params@.Data["q",i]    <-calcQ(object@stock@.Data,index.,error=object@distribution)

    object@logLik[              i]<-calcLogLik(nls.out$fvec,rep(0,length(nls.out$fvec)),error=object@distribution)
    object@rsdlVar[             i]<-nls.out$deviance
    object@dof[,                i]<-summary(nls.out)$df
    object@stats[parNms,,       i]<-summary(nls.out)$coefficients[,c("Std. Error","t value","Pr(>|t|)")]
    object@stopmess[            i]<-nls.out$message
    object@vcov[parNms,parNms, i] <-(summary(nls.out)$cov.unscaled*summary(nls.out)$sigma^2)

    return(object)
	  }

getOptim<-function(object,catch.,index.,nls.out,i){
    parNms                        <-names(nls.out$par)
    object@params@.Data[parNms, i]<-nls.out$par
    #object@stopmess[              i]<-as.character(nls.out$message)
#    object@hessian[parNms,parNms, i]<-nls.out$hessian[parNms,parNms]


    object@vcov[parNms,parNms, i] <--ginv(nls.out$hessian[parNms,parNms])
    iter(object@stock,i)[]        <-c(object@params["K",i])*c(object@params["b0",i])

    stock.                        <-fwdArray(object=iter(object@stock,i)@.Data,model="pellat",catch=catch.,r=c(object@params["r",i]),K=c(object@params["K",i]),msy=c(object@params["msy",i]),p=c(object@params["p",i]))
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

