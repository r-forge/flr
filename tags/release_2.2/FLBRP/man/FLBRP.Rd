\name{FLBRP}
\docType{methods}
\alias{FLBRP}
\title{Method FLBRP}
\description{
  Objects belonging to the \code{\link{FLBRP-class}} can be created using this method. A 
  new object can be based on existing objects of class \code{\link{FLStock-class}} and 
  \code{\link{FLSR-class}}, from which information is extracted and transferred in the
  appropriate format to the \code{FLBRP} object.

  Individual slots can also be filled by using named arguments of the right class. See 
  \code{\link{FLBRP-class}} for slot names and classes.

  The default SR model in an \code{FLBRP} object, when none is provided, is that of mean
  recruitment (\code{rec ~ a}), while the corresponding default for \code{params} is a
  value of 1. For \code{fbar}, the values of F used in the calculations, are taken by 
  default to be \code{seq(0, 4, 0.01)}.

  Slots of class \code{FLQuant} need to match in certain dimensions (see 
  \code{\link{FLBRP-class}}), so the \code{FLBRP} constructors will size correctly those 
  slots for which no object is provided in the argument list.

}
\section{Generic function}{
 FLBRP(object, sr, ...) 
}
\section{Methods}{\describe{
\item{}{}}
}
\author{The FLR Team}
\seealso{}
\examples{
# create an FLBRP from ple4 and some extra arguments
fbrp <- FLBRP(ple4, name='Example FLBRP from ple4')
summary(fbrp)
}
\keyword{methods}
