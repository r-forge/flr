\docType{methods}
\name{tac}
\alias{tac}
\title{Total Allowable Catch}
\arguments{
  \item{\code{object},}{an object of class \code{biodyn}
  or}

  \item{\code{harvest},}{an \code{FLQuant} object with
  harvest rate}
}
\value{
  a \code{FLQuant} object with value(s) for catches
}
\description{
  Calculates the catch for a given harvest rate and stock
  biomass
}
\examples{
#tac("logistic",FLPar(msy=100,k=500))
}
\seealso{
  \code{\link{hcr}}, \code{\link{fwd}}
}

