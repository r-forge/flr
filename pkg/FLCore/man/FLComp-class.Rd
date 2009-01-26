\name{FLComp}
\docType{class}
\alias{FLComp}
\alias{FLComp-class}
\title{Class FLComp}
\description{
  A virtual class that forms the basis for most FLR classes compoed of slots of 
  class \code{\link{FLQuant-class}}. No objects of this class can be constructed.
}
\section{Slots}{\describe{
\item{name (\code{character}):}{Describe slot}
\item{desc (\code{character}):}{Describe slot}
\item{range (\code{numeric}):}{Describe slot}
}}
\section{Extends}{\describe{

}}
\section{Methods}{\describe{
\item{\link{[}(base) :}{Subsetting}
\item{}{\code{ signature(x=FLComp,i=ANY,j=ANY,drop=missing) }}
\item{\link{[<-}(base) :}{Replacement of individual elements}
\item{}{\code{ signature(x=FLComp,i=ANY,j=ANY,value=FLComp) }}
\item{\link{as.data.frame}(base) :}{Convert into a \code{\link{data.frame}}}
\item{}{\code{ signature(x=FLComp,row.names=missing,optional=missing) }}
\item{\link{iter}(FLCore) :}{Select a single iteration (sixth dimension)}
\item{}{\code{ signature(object=FLComp) }}
\item{\link{propagate}(FLCore) :}{Extend along the sixth dimension}
\item{}{\code{ signature(object=FLComp) }}
\item{\link{qapply}(FLCore) :}{Apply a given function over all FLQuant or FLCohort slots}
\item{}{\code{ signature(X=FLComp,FUN=function) }}
\item{\link{summary}(base) :}{Print a summary of the object}
\item{}{\code{ signature(object=FLComp) }}
\item{\link{transform}(base) :}{Substitutes individual slots}
\item{}{\code{signature(_data=FLComp) }}
\item{\link{trim}(FLCore) :}{Trims down object along any dimension}
\item{}{\code{ signature(object=missing) }}
\item{\link{units}(base) :}{Returns a list of units per slot}
\item{}{\code{ signature(x=FLComp) }}
\item{\link{units<-}(base) :}{Modifies the units attribute in one or more slots}
\item{}{\code{ signature(x=FLComp,value=list) }}
\item{\link{window}(stats) :}{Selects and extends along the \emph{year} dimension}
\item{}{\code{ signature(x=FLComp) }}
}}
\author{The FLR Team}
\seealso{ \link{[}, \link{[<-}, \link{as.data.frame}, \link{iter}, \link{propagate}, \link{qapply}, \link{summary}, \link{transform}, \link{trim}, \link{units}, \link{units<-}, \link{window} }
\examples{}
\keyword{classes}
