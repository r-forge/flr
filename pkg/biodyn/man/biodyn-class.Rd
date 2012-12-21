\name{biodyn-class}
\Rdversion{1.1}
\docType{class}
\alias{biodyn-class}

\title{Class \code{"biodyn"}}
\description{
Implements a biomass dynamic model class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("biodyn", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{model}:}{Object of class \code{"factor"} ~~ }
    \item{\code{catch}:}{Object of class \code{"FLQuant"} ~~ }
    \item{\code{stock}:}{Object of class \code{"FLQuant"} ~~ }
    \item{\code{params}:}{Object of class \code{"FLPar"} ~~ }
    \item{\code{bounds}:}{Object of class \code{"array"} ~~ }
    \item{\code{priors}:}{Object of class \code{"array"} ~~ }
    \item{\code{vcov}:}{Object of class \code{"array"} ~~ }
    \item{\code{hessian}:}{Object of class \code{"array"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{desc}:}{Object of class \code{"character"} ~~ }
    \item{\code{range}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{FLComp}"}, directly.
}
\section{Methods}{
  \describe{
    \item{biodyns}{\code{signature(object = "biodyn")}: ... }
    \item{bmsy}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{computeSP}{\code{signature(object = "biodyn", biomass = "FLQuant")}: ... }
    \item{computeSP}{\code{signature(object = "biodyn", biomass = "missing")}: ... }
    \item{computeSP}{\code{signature(object = "biodyn", biomass = "numeric")}: ... }
    \item{fmsy}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "FLQuants")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "list")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "missing")}: ... }
    \item{hcr}{\code{signature(object = "biodyn")}: ... }
    \item{msy}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{plot}{\code{signature(x = "biodyn", y = "missing")}: ... }
    \item{refpts}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{tac}{\code{signature(object = "biodyn")}: ... }
	 }
}

\author{
Laurence Kell

Maintainer: Laurence Kell <laurie.kell@iccat.int>
}

\examples{
showClass("biodyn")
}
\keyword{classes}
