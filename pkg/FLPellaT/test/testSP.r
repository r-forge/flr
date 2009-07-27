library(minpack.lm)
library(FLCore)

################################################################################
## Biomass Dynamic Model in R                                                 ##
################################################################################

#### Get data, and make catch & cpue globally availably in session #############
#alb <-read.table("C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\alb.dat",header=T)
#attach(alb)
myDir  <-"C:\\Stuff\\FLR\\WorkInProgress\\FLSP\\"
test   <-read.csv(paste(myDir,"test.csv",sep=""))
catch  <-FLQuant(test[,"catch"],  dimnames=list(age="all",year=test[,"year"]))@.Data[,,1,1,1,1,drop=T]
stock  <-FLQuant(test[,"biomass"],dimnames=list(age="all",year=test[,"year"]))@.Data[,,1,1,1,1,drop=T]
index  <-FLQuant(test[,"cpue"],   dimnames=list(age="all",year=test[,"year"]))@.Data[,-length(catch),1,1,1,1,drop=T]
rm(test)

####  General functions ########################################################

#### Calculate Q for use in constricted likelihoods etc
calcQ<-function(mnBio,index,error="LOG"){
   if (error=="LOG")
      q <- sum(mnBio*index)/sum(mnBio*mnBio)
   else
      q <- exp(sum(log(index)-log(mnBio))/(length(index)))

   return(q)
   }

### Calculate index hat
indexHat <- function(catch,r,K,b0=1,mpar=2,error="LOG")
   {
   stock <- proj(catch,r,K,b0,mpar)

   ## calculate q
   mnBio<-(stock[-length(stock)]+stock[-1])/2
   q    <-calcQ(mnBio,index,error=error)

   return(q*mnBio)
   }

#### Calculate expected catch for given parameters and stock
catchHat<-function(stock,r,K,mpar=2){
   res<-r*stock*(1-stock^(mpar-1)/K)

   return(res)
   }

#### residuals
residuals<-function(params,b0=1.0,mpar=2.0,error="LOG",catch=catch,index=index)
   {
   r <- params[1]
   K <- params[2]

   index_hat<-indexHat(catch,r,K,b0,mpar,error)
   if (error=="LOG")
      return(c(log(index)-log(index_hat)))
   else
      return(c(index-index_hat))
   }

sigma<-function(index,index_hat,error="LOG"){
   yrs<-dimnames(index)$year[dimnames(index)$year %in%  dimnames(index)$year]

   if (error=="LOG")
      SS   <-sum((log(index[yrs])-log(index_hat))^2)
   else
      SS   <-sum((index[yrs]-index_hat)^2)

   return((SS/length(index_hat))^.5)
   }

####  project biomass for pop parameters and catch
proj<-function(catch,r,K,b0=1,mpar=2){
    stock <- rep(K*b0, length(catch))

    for(y in seq(2, length(catch)))
         stock[y] <- stock[y-1] + r*stock[y-1]*(1-stock[y-1]^(mpar-1)/K) - catch[y]

    stock[stock < 0] <- 1e-10

    return(stock)
    }

#### Reference points
bmsy<-function(K,mpar=2){
  (K/mpar)^(1/(mpar-1))
  }

msy<-function(r,K,mpar=2){
  r*bmsy(K,mpar)*(1-bmsy(K,mpar)^(mpar-1)/K)
  }

fmsy<-function(r,K,mpar=2){
  msy(r,K,mpar)/bmsy(K,mpar)
  }

#### calc LL for Schaefer by default
LL <- function(index,params,b0=1.0,mpar=2,error="LOG")
   {
   r <- params[1]
   K <- params[2]

   index_hat<-indexHat(catch,r,K,b0=b0,mpar=2,error=error)

   if (error=="LOG")
      index=log(index)

   -sum(dnorm(index, index_hat, sigma(index,index_hat,error=error),
                log=(error=="LOG")), na.rm=TRUE)
   }

################################################################################
#### Fit
KGuess<-mean(catch)*10
res<-nls.lm(c(.5,KGuess),residuals,catch=catch,index=index)

#### plot
par(mfrow=c(2,1))
catchability<-calcQ((stock[-1]+stock[-length(stock)])/2,index[-length(stock)])
index_Hat    <-catchability*(stock[-1]+stock[-length(stock)])/2

plot( index)
lines(index_Hat)

plot(catchHat(seq(0,res$par[2],length.out=100),res$par[1],res$par[2])~seq(0,res$par[2],length.out=100),type="l",xlab="biomass",ylab="Yield",ylim=c(0,200))
points(catch~stock,type="b")
points(msy(res$par[1],res$par[2])~bmsy(res$par[2]),type="p",cex=3,col="red",pch=16)

####
#res<-optim(c(.5,1000), LL,lower=rep(1e-8, 4), upper=c(1, Inf),method="L-BFGS-B", control=list(trace=1))
#res<-optim(c(.5,1000), LL,method="SANN", control=list(trace=1))