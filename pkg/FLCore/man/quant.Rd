\name{quant}
\docType{methods}
\alias{quant}
\alias{quant-methods}
\alias{quant,FLArray-method}
\alias{quant<-}
\alias{quant<--methods}
\alias{quant<-,FLArray,missing-method}
\alias{quant<-,FLArray-method}
\alias{quant<-,FLArray,character-method}
\title{Method quant}
\description{
 Function to get or set the name of first dimension (quant) in an object of class \code{FLQuant} or \code{FLCohort}.
}
\section{Generic function}{
 quant(object) 
 quant<-(object,value) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Get the name of the first dimension of an \code{FLQuant} object.}
\item{ signature(object=FLCohort) :}{Get the name of the first dimension of an \code{FLCohort} object.}
\item{ signature(object=FLQuant,value=ANY) :}{Set the name of the first dimension of an \code{FLQuant} object.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
