\name{survprob}
\alias{survprob}
\alias{survprob-methods}
\alias{survprob,FLBiol-method}
\title{Method for calculating survival probabilties given mortaloty in the FLBiol object}
\description{
	For an \code{FLBiol} object with the mortality-at-quant this simply calculates the associated 
	survival probability-at-quant given this information which is used by the \code{r()} method and 
	is also useful for other modelling problems. This can be done by year or by cohort.
}
\usage{
survprob(object, ...)
}
\arguments{
  \item{object}{ An object of type \code{\link{FLBiol-class}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
Calculates the survival probability-at-quant given the mortality information in an \code{FLBiol} 
object - survival probability from one year to the next is simply exp(-M) and the survival probaiblity 
to a given quant is merely the product along the quant dimension of the individual survival probabilties. 
}
\value{
	An object of class \code{\link{FLQuant-class}}.
}
\references{}
\author{FLR Team}
\note{}
\seealso{\code{\link{FLBiol-class}}}
\examples{\dontrun{
data(nsher.biol)
nsh.ps <- survprob(nsh.biol,by='year')
}
}
\keyword{methods}
