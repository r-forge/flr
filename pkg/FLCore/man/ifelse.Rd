\name{ifelse}
\docType{methods}
\alias{ifelse,ANY,FLArray,numeric-method}
\alias{ifelse,ANY,numeric,FLArray-method}
\alias{ifelse,ANY,FLArray,FLArray-method}
\alias{ifelse,FLQuant-method}
\alias{ifelse,FLCohort-method}
\title{Conditional element selection}
\description{
  \code{ifelse} returns a value with the same shape as \emph{test} which is filled with elements selected from either \emph{yes} or \emph{no} depending on whether the element of test is TRUE or FALSE.

  The object returned is of the same class as the FLR-class input. If both \emph{yes} and 
  \emph{emph} are of class \code{\link{FLArray-class}}, they both need to be of the same 
  class: either \code{\link{FLQuant-class}} or \code{\link{FLCohort-class}}.
}
\section{Generic function}{
ifelse(test, yes, no)
}
\section{Method arguments}{\describe{
  \item{test}{an object which can be coerced to logical mode.}
  \item{yes}{an FLArray or numeric for substitution of individual elements of test.}
  \item{no}{an FLArray or numeric for substitution of individual elements of test.}
}}
\value{
  An FLArray with the same shape as \emph{test}, but some or all of its values 
  substituted according to \emph{yes} and \emph{no}.
}
\section{Methods}{\describe{
\item{signature(test=ANY,yes=FLArray,no=numeric):}{Substitutes values in \emph{test} with the corresponding elements in \emph{yes} for which \emph{test} is \code{TRUE}, by \emph{no} otherwise.}
\item{signature(test=ANY,yes=numeric,no=FLArray):}{Substitutes values in \emph{test} with \emph{yes} for those elements where \emph{test} is \code{TRUE}, by the corresponding element in \emph{no} otherwise.}
\item{signature(test=ANY,yes=FLArray,no=numeric):}{Substitutes values in \emph{test} with the corresponding elements in \emph{yes} for which \emph{test} is \code{TRUE}, by those in \emph{no} otherwise.}
}}
\author{The FLR Team}
\seealso{
   \code{\link[base]{ifelse}}
}
\examples{
   data(ple4)
   ifelse(catch.n(ple4) > 50000, 999, catch.n(ple4))
   ifelse(catch.n(ple4) < 1000, 0, catch.n(ple4))
}
\keyword{methods}
