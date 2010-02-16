\name{FLBREM.control}
\alias{FLBREM.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a new FLBREM.control object }
\description{
  This function creates a new control object required by BREM model.
}
\usage{
FLBREM.control(FLBREM = NULL, tol = 1e-09, maxit = 30, min.nse = 0.3, fse = 0.5, rage = 0, qage = 10, shk.n = TRUE, shk.f = TRUE, shk.yrs = 5, shk.ages = 5, window = 100, tsrange = 20, tspower = 3, vpa = FALSE, use.plusgroup = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FLBREM}{ If provided, the 'FLBREM.control' is initialized with the corresponding values of an BREM analysis previously run.
  This is useful for getting the same initial parameters for successive analyses. Specifying one or more of the other arguments 
  supersedes default values, or values obtained from this FLBREM object.
  Following options are vectors that specify min and max bounds on parameters and phase (i.e. order in which they are estimated), -1 means that they are fixed at the
  mean of the min and max values}
  \item{\code{range}:}{ min and max year for the analysis }
	\item{\code{r.mu}:}{ average recruitment }
	\item{\code{r.cv}:}{ cv recruitement logNormal scale }
	\item{\code{r.index.q}:}{ Catchability recruits (survey specific) }
	\item{\code{r.index.cv}:}{ CV of recruit survey errors on log scale }
	\item{\code{adult.q}:}{ Catchability adults (survey specific) }
	\item{\code{adult.cv}:}{ CV of adult survey errors on log scale }
	\item{\code{g0}:}{ log(growth) in year 1 }
	\item{\code{g.sigma}:}{ sd(Z_devs) }
	\item{\code{stock0}:}{ biomass in starting year t=1 }
}
\details{
  See \code{\link{FLBREM}} for more details about the method and some bibliographical references.
}
\value{
  An \code{FLBREM.control} object with same slots as the arguments of the function (except \code{FLBREM})
}

\author{ Laurence Kell & Verena Trenkle }

\seealso{ \code{\link{FLBREM}} }
\examples{
	# To create a new FLBREM.control object with default parameters:
	my.BREM.control <- FLBREM.control()
	my.BREM.control
	# Same, but changing value of recruitment cv
	my.BREM.control <- FLBREM.control(r.cv=c(min=0.1, max=0.3, phase=1))
	my.BREM.control
}
\keyword{ classes }
