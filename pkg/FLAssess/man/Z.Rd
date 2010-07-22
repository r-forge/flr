\name{Z-methods}
\docType{methods}
\alias{Z}
\alias{Z-methods}
\alias{Z,FLStock-method}
\title{Calculates total mortality}
\description{
This method calculates total mortatility from the simple addition of \code{m} and \code{harvest}. 
}
\section{Methods}{
\describe{

\item{\code{Z(obj)}}{Takes in an object of type \code{FLStock} and carries out
the above actions. Returns an object of type \code{FLQuant}}
}}
\examples{
  data(ple4)
  Z(ple4)
}
\keyword{methods}
\keyword{manip}
