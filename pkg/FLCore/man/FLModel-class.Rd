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
  currently available for FLModel, including \code{\link{fmle}}, \code{\link{nls}} and 
  \code{\link{glm}}.
}
\section{Slots}{\describe{
\item{model (\code{formula}):}{Describe slot}
\item{logl (\code{function}):}{Describe slot}
\item{grad (\code{function}):}{Describe slot}
\item{initial (\code{function}):}{Describe slot}
\item{params (\code{FLPar}):}{Describe slot}
\item{logLik (\code{logLik}):}{Describe slot}
\item{vcov (\code{array}):}{Describe slot}
\item{details (\code{list}):}{Describe slot}
\item{name (\code{character}):}{Describe slot}
\item{desc (\code{character}):}{Describe slot}
\item{range (\code{numeric}):}{Describe slot}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=numeric) }}
\item{\link{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=missing) }}
\item{\link{BIC}(stats4) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{fmle}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,start=missing) }}
\item{\link{nls}(stats) :}{Describe method}
\item{}{\code{ signature(formula=FLModel,data=missing,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{AIC}, \link{BIC}, \link{fmle}, \link{nls}, \link{FLComp} }
\examples{}
\keyword{classes}
