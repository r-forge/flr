\name{FLModel}
\docType{class}
\alias{FLModel}
\alias{FLModel-class}
\title{Class FLModel for statistical models}
\description{
  The FLModel class provides a virtual class that developers of various statistical
  models can use to implement classes that allow those models to be tested, fitted
  and presented.

  Slots in this class attempt to map all the usual outputs for a modelling
  exercise, together with the standard inputs. Input data is stored in slots
  created by each of those classes based on FLModel.See, for example
  \code{\link{FLSR-class}} for a class used for stock-recruitment models.

  Various fitting algorithms, similar to those present in the basic R packages are 
  currently available for FLModel, including \code{\link{fmle}}, \code{\link{nls-FLCore}} 
  and \code{\link[stats]{glm}}.
}
\section{Slots}{\describe{
\item{name (\code{character}):}{Name of the object}
\item{desc (\code{character}):}{Description of the object}
\item{range (\code{numeric}):}{Range}
\item{fitted (\code{FLQuant}):}{Estimated values for rec}
\item{residuals (\code{FLQuant}):}{Residuals obtained from the model fit}
\item{covar (\code{FLQuants}):}{Covariates for SR model}
\item{model (\code{formula}):}{Model formula}
\item{grad (\code{function}):}{Function returning the gradient of the model}
\item{lkhd (\code{function}):}{Likelihood function}
\item{initial (\code{function}):}{Function returning initial parameter values for the optimizer}
\item{params (\code{FLPar}):}{Estimated parameter values}
\item{logLik (\code{logLik}):}{Value of the log-likelihood}
\item{vcov (\code{array}):}{Variance-covariance matrix}
\item{details (\code{list}):}{extra information on the model fit procedure}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link[stats]{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=numeric) }}
\item{\link[stats]{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=missing) }}
\item{\link[stats4]{BIC}(stats4) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{fmle}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,start=missing) }}
\item{\link[stats]{nls}(stats) :}{Describe method}
\item{}{\code{ signature(formula=FLModel,data=missing,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) }}
}}
\author{The FLR Team}
\seealso{ \link[stats]{AIC}, \link[stats4]{BIC}, \link{fmle}, \link[stats]{nls}, \link{FLComp} }
\examples{}
\keyword{classes}
