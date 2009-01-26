\name{FLQuant}
\docType{class}
\alias{FLQuant}
\alias{FLQuant-class}
\title{FLQuant class for numerical data}
\description{
  The \code{FLQuant} class is a six-dimensional \code{\link{array}} designed to store most 
  quantitative data used in fisheries and population modelling.

  The sixth dimensions are named. The name of the first dimension can be altered
  by the user from its default, \code{quant}. This could typically be \code{age} or 
  \code{length} for data related to natural populations. Other dimensions are always
  names as follows: \code{year}, for the calendar year of the datapoint; \code{unit}, for 
  any kind of division of the population, e.g. by sex; \code{season}, for any temporal
  strata shorter than year; \code{area}, for any kind of spatial stratification; and 
  \code{iter}, for replicates obtained through bootstrap, simulation or Bayesian analysis.

  In addition, \code{FLQuant} objects contain a \code{units} attribute, of class 
  \code{\link{character}}, intended to contain the units of measurement relevant to the
  data.

}
\section{Slots}{\describe{
\item{.Data (\code{array}):}{A 6-D array for numeric data}
\item{units (\code{character}):}{Units of measurement}
}}
\section{Extends}{\describe{
FLArray
array
structure
matrix
vector
}}
\section{Methods}{\describe{
\item{\link{[}(base) :}{Extract elements}
\item{}{\code{ signature(x=FLQuant,i=ANY,j=ANY,drop=missing) }}
\item{\link{[<-}(base) :}{Substitute elements}
\item{}{\code{ signature(x=FLQuant,i=ANY,j=ANY,value=missing) }}
\item{\link{apply}(base) :}{Operate with a function along certain dimensions}
\item{}{\link[apply,FLQuant,missing,missing-method]{signature(X=FLQuant,MARGIN=missing,FUN=missing)}}
\item{\link{Arith}(base) :}{Arithmetic operations}
\item{}{\code{ signature(e1=FLQuant,e2=FLQuant) }}
\item{\link{as.data.frame}(base) :}{Convert to a data.frame with 7 columns}
\item{}{\code{ signature(x=FLQuant,row.names=ANY,optional=ANY) }}
\item{\link{as.FLQuant}(FLCore) :}{Convert into an FLQuant}
\item{}{\code{ signature(x=missing) }}
\item{\link{barchart}(lattice) :}{Plot a barchart following the given formula}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{bubbles}(FLCore) :}{Plot a bubble following the given formula, with size of the bubbles given by the values in the object}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{bwplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=NULL,to=FLQuant,strict=missing) }}
\item{\link{cv}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{dimnames<-}(base) :}{Describe method}
\item{}{\code{ signature(x=missing,value=missing) }}
\item{\link{dims}(FLCore) :}{Describe method}
\item{}{\code{ signature(obj=missing) }}
\item{\link{discards<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{dotplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{E}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{fec<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{FLCatch}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLCohort}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLMetier}(FLCore) :}{Describe method}
\item{}{\code{ signature(catches=missing) }}
\item{\link{FLQuant}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLQuantPoint}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=missing) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=character) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=function) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=formula) }}
\item{\link{harvest<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{histogram}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{iter}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{iter<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link{iters}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{m<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{m<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{names}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{plot}(graphics) :}{Describe method}
\item{}{\code{ signature(x=FLQuant,y=missing) }}
\item{\link{price<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{print}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{propagate}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{quant}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{quant<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link{quantile}(stats) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=FLQuant,sdlog=FLQuant) }}
\item{\link{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=FLQuant,sdlog=numeric) }}
\item{\link{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=numeric,sdlog=FLQuant) }}
\item{\link{rnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,mean=FLQuant,sd=FLQuant) }}
\item{\link{show}(methods) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{spwn<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{stripplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{summary}(base) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{trim}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{units}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{units<-}(base) :}{Describe method}
\item{}{\code{ signature(x=FLQuant,value=character) }}
\item{\link{window}(stats) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{xyplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
}}
\author{The FLR Team}
\seealso{ \link{[}, \link{[<-}, \link{apply}, \link{Arith}, \link{as.data.frame}, \link{as.FLQuant}, \link{barchart}, \link{bubbles}, \link{bwplot}, \link{catch<-}, \link{catch.n<-}, \link{catch.wt<-}, \link{coerce}, \link{cv}, \link{dimnames<-}, \link{dims}, \link{discards<-}, \link{discards.n<-}, \link{discards.wt<-}, \link{dotplot}, \link{E}, \link{fec<-}, \link{FLCatch}, \link{FLCohort}, \link{FLMetier}, \link{FLQuant}, \link{FLQuantPoint}, \link{FLSR}, \link{harvest<-}, \link{histogram}, \link{iter}, \link{iter<-}, \link{iters}, \link{landings<-}, \link{landings.n<-}, \link{landings.wt<-}, \link{m<-}, \link{n<-}, \link{names}, \link{plot}, \link{price<-}, \link{print}, \link{propagate}, \link{quant}, \link{quant<-}, \link{quantile}, \link{rlnorm}, \link{rnorm}, \link{show}, \link{spwn<-}, \link{stripplot}, \link{summary}, \link{trim}, \link{units}, \link{units<-}, \link{window}, \link{wt<-}, \link{xyplot}, \link{array}, \link{structure}, \link{matrix}, \link{vector}, \link{vector}, \link{vector} }
\examples{}
\keyword{classes}
