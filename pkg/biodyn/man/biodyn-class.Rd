\name{biodyn-class}
\Rdversion{1.1}
\docType{class}
\alias{biodyn-class}
\alias{biodyns,biodyn-method}
\alias{bmsy,biodyn,missing-method}
\alias{computeSP,biodyn,FLQuant-method}
\alias{computeSP,biodyn,numeric-method}
\alias{fmsy,biodyn,missing-method}
\alias{fwd,biodyn,FLQuants-method}
\alias{fwd,biodyn,list-method}
\alias{fwd,biodyn,missing-method}
\alias{harvest,biodyn,ANY-method}
\alias{hcr,biodyn-method}
\alias{hcrJK,biodyn-method}
\alias{msy,biodyn,missing-method}
\alias{refpts,biodyn,missing-method}
\alias{tac,biodyn-method}

\title{Class \code{"biodyn"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("biodyn", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{model}:}{Object of class \code{"factor"} ~~ }
    \item{\code{distribution}:}{Object of class \code{"factor"} ~~ }
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
    \item{computeSP}{\code{signature(object = "biodyn", biomass = "numeric")}: ... }
    \item{fmsy}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "FLQuants")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "list")}: ... }
    \item{fwd}{\code{signature(object = "biodyn", ctrl = "missing")}: ... }
    \item{harvest}{\code{signature(object = "biodyn", catch = "ANY")}: ... }
    \item{hcr}{\code{signature(object = "biodyn")}: ... }
    \item{hcrJK}{\code{signature(object = "biodyn")}: ... }
    \item{msy}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{refpts}{\code{signature(object = "biodyn", params = "missing")}: ... }
    \item{tac}{\code{signature(object = "biodyn")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("biodyn")
}
\keyword{classes}
