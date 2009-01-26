% $ Id: $
\name{sr}
\alias{sr}
\title{Stock-recruitment model function}
\description{
    The \code{sr()} function acts as a front end to the various functions available
    that implement various stocker/recruitment models. Four models are provided in
    this package, but it is possible to write new functions that make use of the \code{sr}
    call mechanism (see below).
}
\usage{
	sr(sr, ...)
}
\arguments{
  \item{sr}{An FLSR object holding the stock and recruitment data (slots r and ssb).}
  \item{\dots}{Other parameters, depending on the model selected.}
}
\details{
    Four commonly used stock-recruitment models are available in this package:
    \item{"ricker":}{ Application of the classical Ricker model.}
    \item{"bevholt":}{ Application of the classical Beverton-Holt model}
    \item{"segreg":}{ Application of the segmented regression/hockey-stick model.}
    \item{"qhstk":}{ Application of the quadratic hockey-stick model.}
    
    The calling mechanism provided by \code{sr} allows for other models to be implemented.
    They would also be called from \code{sr} as long as they comply with the input/output
	interface:

    \item{input}{Both recruitment and SSB will be provided to the model function as vectors
        of the same length as the number of years present in the original FLQuants. No
        interface is provided as yet for models able to estimate S/R relationships by area
        or season. Bayesian models are expected to accept a list with the required prior
        parameter probability distributions. The name of a Bayesian model must contain the
        string "Bayes".}
    \item{output}{The result returned by the function should consist of a list with named
    pairs matching the corresponding slots of a \code{\link{FLSR-class}} object. Class of 
    objects in this list must match those of the \code{\link{FLSR-class}} slots. For example,
    residuals must be a \code{\link{FLQuant-class}}.}

}
\value{
    An object of class \code{\link{FLSR-class}}
}
\references{}
\author{Richard Hillary and Iago Mosqueira}
\note{}
\seealso{\code{\link{FLSR-class}}}
\examples{
}
\keyword{models}
