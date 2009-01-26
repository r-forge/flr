\name{lattice}
\docType{methods}
\alias{barchart,ANY,ANY-method}
\alias{barchart,formula,FLQuant-method}
\alias{barchart,formula,FLComp-method}
\alias{bwplot,ANY,ANY-method}
\alias{bwplot,formula,FLQuant-method}
\alias{bwplot,formula,FLComp-method}
\alias{densityplot,ANY,ANY-method}
\alias{densityplot,formula,FLPar-method}
\alias{dotplot,ANY,ANY-method}
\alias{dotplot,formula,FLQuant-method}
\alias{dotplot,formula,FLComp-method}
\alias{histogram,ANY,ANY-method}
\alias{histogram,formula,FLQuant-method}
\alias{histogram,formula,FLQuants-method}
\alias{histogram,formula,FLComp-method}
\alias{histogram,formula,FLPar-method}
\alias{xyplot,ANY,ANY-method}
\alias{xyplot,formula,FLQuant-method}
\alias{xyplot,formula,FLQuants-method}
\alias{xyplot,formula,FLCohort-method}
\alias{xyplot,formula,FLComp-method}
\title{Method barchart}
\description{
 Implementation of Trellis graphics in FLR. Plot methods in the lattice package are 
 available for object of class \code{FLQuant} or  \code{FLQuants}.
}
\section{Generic function}{
 barchart(x,data) 

 bwplot(x,data) 

 densityplot(x,data) 

 dotplot(x,data) 

 histogram(x,data) 

 xyplot(x,data) 
}
\section{Methods}{\describe{
\item{ signature(x=ANY, data=ANY) :}{Describe method}
\item{ signature(x=formula, data=FLQuant) :}{Use the lattice functionality for objects of class FLQuant}
\item{ signature(x=formula,data=FLPar) :}{Use the lattice functionality for objects of class FLPar}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
