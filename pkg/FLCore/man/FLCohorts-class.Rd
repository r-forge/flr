\name{FLCohorts}
\docType{class}
\alias{FLCohorts}
\alias{FLCohorts-class}
\title{Class FLCohorts}
\description{
\code{FLCohorts} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLCohorts} must all be of class \code{FLCohort}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
}
\section{Slots}{\describe{
\item{.Data (\code{list}):}{The data.}
\item{names (\code{character}):}{Names of the list elements.}
\item{desc (\code{character}):}{Description of the object.}
\item{lock (\code{logical}):}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements.}
}}
\section{Extends}{\describe{
FLlst
list
vector
}}
\section{Methods}{\describe{
\item{\link{*}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLCohorts,e2=FLCohorts) }}
\item{\link{Arith}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLCohorts,e2=FLCohorts) }}
\item{\link{as.data.frame}(base) :}{Describe method}
\item{}{\code{ signature(x=FLCohorts,row.names=missing,optional=missing) }}
\item{\link{bubbles}(FLCore) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLCohorts) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLCohorts) }}
\item{\link{iter}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{model.frame}(stats) :}{Describe method}
\item{}{\code{ signature(formula=missing) }}
\item{\link{show}(methods) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{summary}(base) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{xyplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLCohorts) }}
}}
\author{The FLR Team}
\seealso{ \link{*}, \link{Arith}, \link{as.data.frame}, \link{bubbles}, \link{catch<-}, \link{iter}, \link{model.frame}, \link{show}, \link{summary}, \link{xyplot}, \link{FLlst}, \link{list}, \link{vector} }
\examples{}
\keyword{classes}
