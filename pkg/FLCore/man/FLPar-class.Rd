\name{FLPar}
\docType{class}
\alias{FLPar}
\alias{FLPar-class}
\title{Class FLPar}
\description{
 The \code{FLPar} class is a class for storing the parameters of a model. It is based on 
 the array class which can store Monte Carlo samples and the names of the relevant 
 parameter vectors.

 Methods for this class include subsetting and replacement as they exist for the 
 \code{FLQuant} class. There are methods for extracting statistics of the sample (mean, 
 median etc.) and for plotting the parameter samples.
}
\section{Slots}{\describe{
\item{.Data (\code{array}):}{Describe slot}
}}
\section{Extends}{\describe{

}}
\section{Methods}{\describe{
\item{\link{[}(base) :}{Subsetting method to access the parameter values.}
\item{}{\code{ signature(x=FLPar,i=ANY,j=ANY,drop=missing) }}
\item{\link{[<-}(base) :}{Replacement method for the parameter values.}
\item{}{\code{ signature(x=FLPar,i=ANY,j=ANY,value=missing) }}
\item{\link{as.data.frame}(base) :}{Creates a data frame from the object.}
\item{}{\code{ signature(x=FLPar,row.names=ANY,optional=ANY) }}
\item{\link{densityplot}(lattice) :}{applies the densityplot method from \code{lattice}}
\item{}{\code{ signature(x=formula,data=FLPar) }}
\item{\link{histogram}(lattice) :}{applies the histogram method from \code{lattice}}
\item{}{\code{ signature(x=formula,data=FLPar) }}
\item{\link{iter}(FLCore) :}{extracts the relevant iteration for the given parameter sample.}
\item{}{\code{ signature(object=missing) }}
\item{\link{iter<-}(FLCore) :}{replacement method for the parameter iteration.}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link{mean}(base) :}{calculates the mean of the parameter samples.}
\item{}{\code{ signature(x=missing) }}
\item{\link{median}(stats) :}{calculates the median of the parameter samples.}
\item{}{\code{ signature(x=missing,na.rm=missing) }}
\item{\link{plot}(graphics) :}{default plot method for \code{FLPar}}
\item{}{\code{ signature(x=FLPar,y=missing) }}
\item{\link{splom}(lattice) :}{applies the splom method from \code{lattice}}
\item{}{\code{ signature(x=FLPar,data=missing) }}
\item{\link{summary}(base) :}{Summarises the \code{FLPar} object.}
\item{}{\code{ signature(object=missing) }}
\item{\link{units}(base) :}{extracts the units of the parameters.}
\item{}{\code{ signature(x=missing) }}
\item{\link{units<-}(base) :}{replacement method for the units of the object.}
\item{}{\code{ signature(x=FLPar,value=character) }}
\item{\link{var}(stats) :}{calculates the variance of the parameter samples.}
\item{}{\code{ signature(x=missing,y=missing,na.rm=missing,use=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{[}, \link{[<-}, \link{as.data.frame}, \link{densityplot}, \link{histogram}, \link{iter}, \link{iter<-}, \link{mean}, \link{median}, \link{plot}, \link{splom}, \link{summary}, \link{units}, \link{units<-}, \link{var} }
\examples{}
\keyword{classes}
