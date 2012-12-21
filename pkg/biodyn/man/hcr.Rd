\docType{methods}
\name{hcr}
\alias{hcr}
\alias{hcr,biodyn-method}

\title{Harvest Control Rule}
\arguments{
  \item{\code{object},}{an object of class \code{biodyn}
  or}

  \item{\code{ctrl},}{a string or factor that species the
  model}

  \item{\code{msy},}{an \code{FLPar} object with model
  parameters}
}
\value{
  a \code{FLPar} object with value(s) for TAC
}
\description{
  Calculates catch quota or Total Allowable Catch (TAC)
  based on a hockey stock harvest control rule.
}
\examples{
#hcr("logistic",FLPar(msy=100,k=500))
}
\seealso{
  \code{\link{msy}}, \code{\link{bmsy}}, \code{\link{fmsy}}
  and \code{\link{refpts}}
}

