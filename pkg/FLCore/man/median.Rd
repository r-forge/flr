\name{median}
\docType{methods}
\alias{median,ANY,missing-method}
\alias{median,FLPar,missing-method}
\alias{median,FLPar-method}
\title{Method median}
\description{
Calculates the median.  Can be used directly on an object or with apply etc.
}
\section{Generic function}{
 median(x,na.rm) 
}
\section{Methods}{\describe{
\item{ signature(x=ANY,na.rm=missing) :}{Returns the median of x}
\item{ signature(x=FLQuantPoint,na.rm=missing) :}{Returns the median of x}
\item{ signature(x=FLQuant,na.rm=missing) :}{Returns the median of x}
\item{ signature(x=FLPar,na.rm=missing) :}{Returns the median of x}
}}
\author{The FLR Team}
\seealso{ 
\link{median}
\link{apply}}
\examples{
data(ple4)
median(stock.n(ple4)[,"2001"])
apply(catch.n(ple4), 2, median)
}
\keyword{methods}
