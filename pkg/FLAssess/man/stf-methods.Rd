\name{stf-methods}
\docType{methods}
\alias{stf}
\alias{stf-methods}
\alias{stf,FLAssess-method}
\alias{stf,FLBiol-method}
\alias{stf,FLStock-method}
\title{Projection preparation for FLR}

\description{
Extends an FLStock object along the year dimensin and prepares some of the slots
for forward stock projection, using a similar method
to FLSTF.  Can be used in conjunction with fwd() for validating assessments.
}
\section{Generic Function}{
\describe{
\item{Usage}{stf(object,nyrs=3,wts.nyrs=3,fbar.nyrs=NA,disc.nyrs=NA,arith.mn=TRUE, na.rm=TRUE)}
\item{Arguments:}{\ }
\describe{
  \item{\ }{\code{object}: \code{FLStock} object the forecast is to be perfomed on.}
  \item{\ }{\code{nyrs}: Number of years to extend the stock object by, i.e. number of
              forecast years.}
  \item{\ }{\code{wts.nyrs}: Number of years over which to calculate mean for *.wt,
                  *.spwn, mat and m slots.}
  \item{\ }{\code{fbar.nyrs}: Number of years over which to calculate mean for harvest slot.}
	\item{disc.nyrs}{Number of years over which to calculate mean for discards.n
                    and landings.n slots, If unspecified, wts.nyrs is used.}
	\item{arith.mn}{Boolean - if TRUE use arithmetic mean, if FALSE use geometric
                  mean.}
  \item{\ }{\code{na.rm}: Boolean - whether to ignore NA values when calculating means.}
}
\item{Value}{\code{FLStock} object}
}}

\section{Methods}{
\describe{
\item{object="FLStock"}{: Extends the stock object by a number of years and estimates the values in the
future years of the slots.

The future values of the mat, m, harvest.spwn, m.spwn, harvest and all *.wt
slots are set to the mean (geometric or arithmetic) value of the last user
defined number of years.

The future values of the discards.n and landings.n slots are returned only as
estimated proportions, not actual numbers.  The proportions are calculated as
the mean proportion of the sum of the landings and discard numbers of the last
user defined number of years.

The remaining slots (stock.n, catch.n, stock, catch, discards, landings) are
set to NA.

Not yet implemented for multiple seasons or areas.}
}
}

\seealso{
\code{\link{FLAssess-class}}, \code{\link[FLCore]{FLStock-class}}
}


\examples{
# load ple4
data(ple4)
# stf with default settings
ple4.stf <- stf(ple4,nyrs=3, wts.nyrs=4, fbar.nyrs=3)
}
\author{
	FLR Team
}
\keyword{methods}
