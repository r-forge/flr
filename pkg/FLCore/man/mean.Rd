\name{mean}
\docType{methods}
\alias{mean,FLPar-method}
\alias{mean,FLQuant-method}
\title{Method mean}
\description{
Calculates the mean.  Can be used directly on an object or with apply etc.
}
\section{Generic function}{
 mean(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLPar) :}{Returns the mean of x}
\item{ signature(x=FLQuant) :}{Returns the mean of x}
\item{ signature(x=FLQuantPoint) :}{Returns the mean of x}
}}
\author{The FLR Team}
\seealso{
\link{median}
\link{apply}}
\examples{
data(ple4)
mean(stock.n(ple4)[,"2001"])
apply(catch.n(ple4), 2, mean)
}
\keyword{methods}
