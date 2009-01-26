\name{FLQuantPoint}
\docType{class}
\alias{FLQuantPoint}
\alias{FLQuantPoint-class}
\title{Class FLQuantPoint}
\description{
  The \code{FLQuantPoint} class summarizes the contents of an \code{FLQuant} object with
  multiple iterations along its sixth dimension using a number of descriptive statistics.

  An object of this class has a set structure along its sixth dimension (\emph{iter}), 
  which will always be of length 5, and with dimnames \code{mean},  \code{median},
  \code{var}, \code{uppq} and \code{lowq}. They refer, respectively, to the sample mean,
  sample median, variance, and lower (0.25) and upper (0.75) quantiles.
  
  Objects of this class wil be typically created from an \code{FLQuant}. The various
  statistics are calculated along the \emph{iter} dimension of the original 
  \code{FLQuant} using \code{\link{apply}}.

}
\section{Slots}{\describe{
\item{.Data (\code{array}):}{The main array holding the computed statistics}
\item{units (\code{character}):}{Units of measurement}
}}
\section{Extends}{\describe{
FLQuant
array
}}
\section{Methods}{\describe{
\item{\link{lowq}(FLCore) :}{Accesor for \code{lowq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{lowq<-}(FLCore) :}{Modifies the \code{lowq} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link{mean}(base) :}{Accesor for \code{mean} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{mean<-}(FLCore) :}{Modifies the \code{mean} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link{median}(stats) :}{Accesor for \code{median} iteration}
\item{}{\code{ signature(x=FLQuantPoint,na.rm=missing) }}
\item{\link{median<-}(FLCore) :}{Modifies the \code{median} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link{plot}(graphics) :}{Plot an object of the class}
\item{}{\code{ signature(x=FLQuantPoint,y=missing) }}
\item{\link{quantile}(stats) :}{Accesor for \code{lowq} or \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{rlnorm}(stats) :}{Creates a lognormally-distributed \code{FLQuant} using the information on an \code{FLQuantPoint}}
\item{}{\code{ signature(n=numeric,meanlog=FLQuantPoint,sdlog=missing) }}
\item{\link{rnorm}(stats) :}{Creates a normally-distributed \code{FLQuant} using the information on an \code{FLQuantPoint}}
\item{}{\code{ signature(n=numeric,mean=FLQuantPoint,sd=missing) }}
\item{\link{show}(methods) :}{Outputs object in screen}
\item{}{\code{ signature(object=FLQuantPoint) }}
\item{\link{summary}(base) :}{Prints a summary of the object}
\item{}{\code{ signature(object=FLQuantPoint) }}
\item{\link{uppq}(FLCore) :}{Accesor for \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{uppq<-}(FLCore) :}{Modifies the \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link{var}(stats) :}{Accesor for \code{var} iteration}
\item{}{\code{ signature(x=FLQuantPoint,y=missing,na.rm=missing,use=missing) }}
\item{\link{var<-}(FLCore) :}{Modifies the \code{var} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
}}
\author{The FLR Team}
\seealso{ \link{lowq}, \link{lowq<-}, \link{mean}, \link{mean<-}, \link{median}, \link{median<-}, \link{plot}, \link{quantile}, \link{rlnorm}, \link{rnorm}, \link{show}, \link{summary}, \link{uppq}, \link{uppq<-}, \link{var}, \link{var<-}, \link{FLQuant}, \link{array}, \link{structure}, \link{matrix}, \link{vector}, \link{vector}, \link{vector} }
\examples{
flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
flqp <- FLQuantPoint(flq)
summary(flqp)
mean(flqp)
var(flqp)
rnorm(200, flqp)
}
\keyword{classes}
