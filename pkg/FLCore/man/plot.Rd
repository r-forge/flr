\name{plot}
\docType{methods}
\alias{plot,FLQuant,missing-method}
\alias{plot,FLQuantPoint,missing-method}
\alias{plot,FLPar,missing-method}
\alias{plot,FLStock,missing-method}
\alias{plot,FLStocks,missing-method}
\alias{plot,FLStocks,FLPar-method}
\alias{plot,FLBiol,missing-method}
\alias{plot,FLCohort,missing-method}
\alias{plot,FLIndex,missing-method}
\alias{plot,FLIndices,missing-method}
\alias{plot,FLSR,missing-method}
\title{Method plot}
\description{
  Standard plot methods for every FLR class. FLR plot methods are based on
  \code{\link[lattice]{lattice}}, and attempt to show a general view of the object 
  contents.

  Users are encouraged to write their own plotting code make use of the overloaded
  \code{\link[base]{lattice}} methods, for example \code{\link[base]{xyplot}} or
  \code{\link[base]{bwplot}}. See also \code{\link{lattice-FLCore}}.
}
\section{Generic function}{
 plot(x,y) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant,y=missing) :}{Plot of an \emph{FLQuant} conditioned on all dimension of length > 1.}
\item{ signature(x=FLQuantPoint,y=missing) :}{Box and whiskers plot of the yearly time series, conditioned on all dimension of length > 1.}
\item{ signature(x=FLPar,y=missing) :}{Describe method}
\item{ signature(x=FLStock,y=missing) :}{Describe method}
\item{ signature(x=FLStocks,y=missing) :}{Describe method}
\item{ signature(x=FLBiol,y=missing) :}{Describe method}
\item{ signature(x=FLCohort,y=missing) :}{Describe method}
\item{ signature(x=FLIndex,y=missing) :}{Describe method}
\item{ signature(x=FLSR,y=missing) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link[graphics]{plot} }
\examples{
plot(catch.n(ple4)[, 1:20])
plot(catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)

plot(ple4)
}
\keyword{methods}
