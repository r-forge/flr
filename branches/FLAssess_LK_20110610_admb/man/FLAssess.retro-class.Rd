\name{FLAssess.retro-class}
\docType{class}
\alias{FLAssess.retro-class}
\alias{plot,FLAssess.retro,missing-method}
\alias{as.data.frame,FLAssess.retro-method}
\alias{window,FLAssess.retro-method}
\alias{as.data.frame,FLAssess.retro,ANY,ANY-method}

\title{Class FLAssess.retro}
\description{

WARNING: This method has now been deprecated and is no longer maintained.
Users are encouraged to perform retrospective analysis using tapply.
See below for an example.

A class for results of a retrospective analysis}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLAssess.retro", ...)}, but they are usually created by a call to the \code{\link{retro}} function.
}
\section{Slots}{
	 \describe{
    \item{\code{desc}:}{Object of class \code{"character"}.  Object description.}
    \item{\code{ssb}:}{Object of class \code{"list"}. A list of FLQuants to hold
    SSB data for each retrospective analysis}
    \item{\code{recruits}:}{Object of class \code{"list"}, A list of FLQuants to hold
    recruitment data for each retrospective analysis}
    \item{\code{harvest}:}{Object of class \code{"list"}. A list of FLQuants to hold
    harvest data for each retrospective analysis}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "FLAssess.retro", y = "missing")}: ... }
    \item{as.data.frame}{\code{signature(x = "FLAssess.retro")}: ... }
    \item{window}{\code{signature(x = "FLAssess.retro")}: ... }
	\item{as.data.frame}{Returns a data.frame version of the object}
	 }
}
\author{Laurie Kell}
\seealso{
	\code{\link{retro}}, \code{\link{FLAssess-class}}
}
\examples{
}
\keyword{classes}
