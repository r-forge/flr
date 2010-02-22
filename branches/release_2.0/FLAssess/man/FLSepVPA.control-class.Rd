\name{FLSepVPA.control-class}
\docType{class}
\alias{FLSepVPA.control-class}
\alias{FLSepVPA.control}

\title{Control class for FLSepVPA}
\description{
    Objects of this class contain all the neccessary settings for the Separable VPA
    model available through function \code{SepVPA}.

}
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("FLSepVPA.control", ...)} or
    by calling the \code{\link{FLSepVPA.control}} function.
}
\section{Slots}{
	 \describe{
    \item{\code{sep.nyr}:}{Object of class \code{"integer"}. Number of years for separable model.}
    \item{\code{sep.age}:}{Object of class \code{"integer"}. Reference age for fitting the separable model. Default value = 4}
    \item{\code{sep.sel}:}{Object of class \code{"numeric"}. Default value = 1.0}
  }
}
\section{Methods}{
No methods defined with class "FLSepVPA.control" in the signature.
}
\references{
JG Shepherd, SM Stevens. 1983. Separable VPA: User's guide - Int. Rep., MAFF Direct. Fish. Res.
}
\author{Iago Mosqueira}
\seealso{
    \code{FLSepVPA}
}
\examples{
sep.vpa.control <- FLSepVPA.control(sep.nyr=5, sep.age=5)
}
\keyword{classes}
