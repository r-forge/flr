\name{FLICA.control}
\alias{FLICA.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a new FLICA.control object }
\description{
  This function creates a new control object required by ICA model (Integrated Catch-at-age analysis).
}
\usage{
FLICA.control(FLICA = NULL, sep.nyr = "missing", sep.age = "missing", sep.sel = "missing", sr = "missing", sr.age = "missing", lambda.age = "missing", lambda.yr = "missing", lambda.sr = "missing", index.model = "missing", index.cor = "missing")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FLICA}{ If provided, the 'FLICA.control' is initialized with the corresponding values of an ICA analysis previously run.
  This is useful for getting the same initial parameters for successive analyses. Specifying one or more of the other arguments 
  supersedes default values, or values obtained from this FLICA object}  
  \item{sep.nyr}{ Number of years for separable model }
  \item{sep.age}{ Reference age for fitting the separable model }
  \item{sep.sel}{ Selection on last true reference age }
  \item{sr}{ Stock and recruitment parameters, i.e. whether to fit a stock-recruit relation }
  \item{sr.age}{ age at recruitment  }
  \item{lambda.age}{ Weighting matrices for catch-at-age; for aged surveys; for SSB surveys }
  \item{lambda.yr}{ Relative weights by year }
  \item{lambda.sr}{ weight for the SRR term in the objective function }
  \item{index.model}{ Catcahability model: 'p' N=aU^b, 'l' N=aU, 'a' N = U }
  \item{index.cor}{ Are the age-structured indices are correlated across ages }
}
\details{
  See \code{\link{FLICA}} for more details about the method and some bibliographical references.
}
\value{
  An \code{FLICA.control} object with same slots as the arguments of the function (except \code{FLICA})
}

\author{ Laurence Kell }

\seealso{ \code{\link{FLICA}} }
\examples{
	# To create a new FLICA.control object with default parameters:
}
\keyword{ classes }
