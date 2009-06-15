% --- Source file: man/fwdTarget.Rd ---
\name{fwdTarget}
\alias{fwdTarget}

\title{Targets for fwd}
\description{
  \code{fwdTarget} {creates a target object that sets targets for species, fleets
   and metiers.}
}
\usage{
fwdTarget(list(year=1998:2000,value=c(50000,51000,52000),quantity=rep("landings",3)))
}
\arguments{
  \item{list: a list with the following items, defaults provided for those ommitted}
  \item{year: the year for which the target is being found or the constraint is being set}
  \item{value: the value for the target or constraint}
  \item{min: a lower bound for the target or constraint}
  \item{max: } {an upper bound for the target or constraint}
  \item{quantity: } {a factor corresponding to the quantity type e.g. "ssb", "catch", "f"}
  \item{rel: } {in the case of a relative target, this will be set to an earlier year}
  \item{fleet: } {code for fleet for which target/constraint is being applied}
  \item{metier: } {code for metier for which target/constraint is being applied}
  \item{spp: } {code for spp for which target/constraint is being applied}
  }
\details{
  \code{fwdTarget} {allows a range of target options to be set for stocks and fisheries,
  particularly useful for mixed stock/fisheries.}
}
\value{
  A data frame with columns for year etc, and rows for year and fleet/metier/spp
  combinations.
}
\seealso{
  \code{\link{fwdControl}}, \code{\link{fwd}}
}
\author{
  Laurence Kell, Finlay Scott & Iago Mosqueira
  }
\examples{
\dontrun{target<-fwdTarget(list(year    =rep(1995:2000),value=c(.40,100000,5000,120000,220000,0.65),
                       quantity=c("f", "ssb","catch","ssb","ssb","f")))

}}

\keyword{fwd}
\keyword{fwdTarget}

\eof
