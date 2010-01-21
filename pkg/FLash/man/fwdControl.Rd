% --- Source file: man/fwdControl.Rd ---
\name{fwdControl}
\alias{fwdControl}

\title{Controls for fwd}
\description{
  \code{fwdControl} {creates a control object that determines effort by fleet and metier
   and metiers.}
}
\usage{
fwdControl(list(year,fleet,metier,value,rel.yr,rel.fleet,rel.metier,min,max,rel.bound))
}
\arguments{
  \item{year: } {the year for which the effort value is being controlled/found}
  \item{fleet: } {the fleet for which the effort value is being controlled/found}
  \item{metier: } {the year for which the effort value is being controlled/found}
  \item{value: } {if !na then effort fixed at this value}
  \item{rel.yr: } {if !na and value !=na then the value in 'value' is a relative to this year}
  \item{rel.fleet: } {if !na and value !=na then the value in 'value' is relative to this fleet (i.e. 50\% would be 0.5)}
  \item{rel.metier: } {if !na and value !=na then the value in 'value' is relative to this metier (i.e. 50\% would be 0.5)}
  \item{irel.metier: } {if !=NA and rel.fleet!=NA then value relative to this fleet*metier}
  \item{min: } {lower bound}
  \item{max: } {upper bound}
  \item{rel.bound: } {are min and max absolute or relative}
  }
\details{
  \code{fwdControl} {allows a range of Control options to be set for stocks and fisheries,
  particularly useful for mixed stock/fisheries.
  The control object specifies how to estimate effort by fleet*metier,

If value =NA then fwd estimates it, if it is a valid value then it is fixed. Min and max are bounds. Therefore you can't have values for min, max and value so the following is invalid

\code{fwdControl(year=1999:2000,value=.2,min=.1,max=.6)}

but the following are all valid but currently fail validity

\code{#fix effort
fwdControl(year=1999:2000,value=.2,min=NA,max=NA)

#estimate effort
fwdControl(year=1999:2000,value=NA,min=NA,max=NA)

#estimate effort but lower bound on effort
fwdControl(year=1999:2000,value=NA,min=.1,max=NA)

#estimate effort but upper bound on effort
fwdControl(year=1999:2000,value=.2,min=NA,max=.6)}
}
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
\dontrun{
Control<-fwdControl(data.frame(year    =rep(1995:2000),value=c(.40,100000,5000,120000,220000,0.65),
                       quantity=c("f", "ssb","catch","ssb","ssb","f")))

}}

\keyword{fwd}
\keyword{fwdControl}
