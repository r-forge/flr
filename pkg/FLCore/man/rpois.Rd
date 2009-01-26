\name{rpois}
\docType{methods}
\alias{rpois,ANY,ANY,ANY-method}
\alias{rpois,numeric,FLQuant-method}
\title{Method rpois}
\description{
  Generates random numbers following a Poissom distribution. lambda, the (non-negative)
  means can be specified as an FLQuant object.
}
\section{Generic function}{
 rpois(n, lambda) 
}
\section{Methods}{\describe{
\item{ signature(n=numeric, lambda=FLQuant) :}{Generate a Poisson-distributed FLQuant
  object with 'n' iters.}
}}
\author{The FLR Team}
\seealso{ \link{FLQuant} }
\examples{
   rpois(10, lambda=FLQuant(1:10, dim=c(10,20,3)))
}
\keyword{methods}
