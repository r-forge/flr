\name{plot}
\docType{methods}
\alias{plot,FLAssess-methods}
\title{Plot method for FLAssess class}
\description{
  Standard plot methods for the \code{FLAssess} class. FLR plot methods are based on
  \code{\link[lattice]{lattice}}, and attempt to show a general view of the object 
  contents.

  Users are encouraged to write their own plotting code make use of the overloaded
  \code{\link[lattice]{lattice}} methods, for example \code{\link[lattice]{xyplot}} or
  \code{\link[lattice]{bwplot}}. See also \code{\link[FLCore]{lattice-FLCore}}.
}
\section{Methods}{
\describe{
  \item{ signature(x=FLAssess,y=missing) :}{Plot of an \emph{FLAssess} object.}
}}
\author{The FLR Team}
\seealso{ \link[graphics]{plot} }
\keyword{methods}
\keyword{hplot}
