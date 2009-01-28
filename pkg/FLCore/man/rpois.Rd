\name{rpois}
\docType{methods}
\alias{rpois,numeric,FLQuant-method}
\name{rnorm}
\docType{methods}
\alias{rnorm,numeric,FLQuant,FLQuant-method}
\alias{rnorm,numeric,FLQuant,numeric-method}
\alias{rnorm,numeric,numeric,FLQuant-method}
\alias{rnorm,numeric,FLQuantPoint,missing-method}
\title{Method rnorm}
\description{
  Generates random numbers following a normal distribution. mean and sd can be specified
  as two FLQuant objects, of the same dimensions, but any of the two could be given as
  a numeric. In this case the value will be reused accordingly.
}
\section{Generic function}{
 rnorm(n, mean, sd) 
}
\section{Methods}{\describe{
\item{ signature(n=numeric,mean=FLQuant,sd=FLQuant) :}{\code{n} is the number of iterations of the return object. \code{mean} and \code{sd} are FLQuant objects and must be of equal size.  When both \code{sd} and \code{mean} are specified the returned object is an FLQuant object with \code{n} iterations, filled with randomly generated numbers.  When only 1 of \code{mean} or \code{sd} is supplied the return object is a vector of length \code{n}.}
\item{ signature(n=numeric,mean=numeric,sd=FLQuant) :}{same as above, but dimensions will
  be given by sd, and mean will be reused accordingly.}
\item{ signature(n=numeric,mean=FLQuant,sd=numeric) :}{same as above, but instead 
  dimensions will be given by mean, and sd will be reused accordingly.}
\item{ signature(n=numeric,mean=FLQuantPoint,sd=missing) :}{uses an FLQuantPoint to obtain
  mean and sd from the mean and var iterations.}
}}
\author{The FLR Team}
\seealso{ \link{FLQuant} }
\examples{
   data(ple4)
   rnorm(10,mean=harvest(ple4)[,"2001"], sd=harvest(ple4)[,"2001"])
}
\keyword{methods}
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
