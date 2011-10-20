\name{FLBioDym}
\docType{class}
\alias{FLBioDym-class}
\alias{FLBioDym}
\alias{FLBioDym-methods}
\alias{FLBioDym,ANY-method}
\alias{FLBioDym,missing-method}
\title{Class FLBioDym}
\description{
  Class for biomass dynamic assessment models. Input data required are catch biomass
  and an index of abundance (currently only one), missing values allowed in the index.

  A series of commonly-used production functions are avilable, including Pella Tomlinson,
  Schaefer, Fox, Shepherd, Fletcher and Gulland. Estimates of MSY reference points are
  available with uncertainty derived by the delta method.
}
\section{Slots}{\describe{
\item{name}{Name of the object. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{range}{Range. \code{numeric}.}
\item{catch}{Catch biomass series. \code{FLQuant}.}
\item{index}{Index of abundance. \code{FLQuant}.}
\item{stock}{Stock biomass. \code{FLQuant}.}
\item{model}{Model. \code{character}.}
\item{distribution}{Error distriution for index. \code{character}.}
\item{params}{Estimated parameter values. \code{FLPar}.}
\item{logLik}{Value of the log-likelihood. \code{logLik}.}
\item{vcov}{Variance-covariance matrix. \code{array}.}
\item{hessian}{Hessian matrix. \code{array}.}
\item{rsdlVar}{Residual Variance. \code{numeric}.}
\item{dof}{Degress of freedom. \code{array}.}
\item{details}{extra information on the model fit procedure. \code{list}.}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLBioDym(object)} constructor method allows simple creation of new FLBioDym with the methods described below.
}
\item{ signature(model=missing) :}{Creates an empty \code{FLBioDym} object.}
\item{ signature(catch=FLQuant,index=FLQuant) :}{Creates an \code{FLBioDym} from catch and cpue time series.}
\item{ signature(model=formula) :}{Creates an \code{FLBioDym} object with the given formula in \code{model} slot.}
}}
\section{Constructor details}{\describe{
Any of the slots of the \code{FLBioDym} class can be used as argument in (...), in which case the corresponding slot is filled with the given argument.

If no 'FLQuant' slots are supplied in the call to the constructor 'FLBioDym()', an 'FLBioDym' object with dimension '(1,1,1,1,1,1)' in the 'FLQuant' slots is returned. Otherwise if one of the arguments in (...) is an 'FLQuant', an 'FLBioDym' object which slots have the same dimension as the 'FLQuant' argument is returned. If two ore more 'FLQuant' slots are supplied in the call to the function, they must have the same dimension.
}}

\section{Methods}{\describe{
All methods are inherited from \code{\link{FLModel-class}}, except for
\item{\link[graphics]{plot}(graphics) :}{Plot an FLBioDym object}
\item{}{\code{ signature(x=FLBioDym,y=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{FLModel}, \link{FLComp} }
\examples{
# FLBioDym - A package for fitting biomass dynamic models
library(FLBioDym)

# load up example data set from
# Polacheck,T., Hilborn,R., and Punt.A.E, Fitting Surplus Production Models:
# Comparing Methods and Measuring Uncertainty.
# Can. J. Fish. Aquat. Sci. 50(12): 2597–2607 (1993)
data(albBD)

# Inspect the object
summary(albBD)

# fit
albBD <- admbBD(albBD)
#albBD <- admbBD(albBD, start=c(r=0.3,K=mean(catch(albBD))*10))

# Population estimates
plot(albBD)

# parameters
params(albBD)

# Reference points
refpts(albBD)
msy( albBD)
bmsy(albBD)
fmsy(albBD)

# covariance estimates
vcov(albBD)

# Likilihood profile
r.  <-seq(0.1, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(albBD,fixed=c(r=x),start=c(r=.3, K=mean(catch(albBD))*10))@logLik})
plot(logl~r.,type="l")
points(c(albBD@params["r",]),albBD@logLik,pch=16,col="red",cex=2)

# Uncertainty in reference points
refptSE(albBD)

# Jacknife
albJK<-albBD
index(albJK)<-jacknife(index(albJK))
albJK<-fit(albJK,start=c(r=0.3,K=mean(catch(albJK))*10))

plot(albJK)

}
\keyword{classes}


