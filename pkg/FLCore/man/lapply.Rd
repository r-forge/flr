\name{lapply}
\docType{methods}
\alias{lapply,ANY,missing-method}
\alias{lapply,FLlst,missing-method}
\alias{lapply,FLlst-method}
\title{Method lapply}
\description{
 lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X. 
}
\section{Generic function}{
 lapply(X,FUN) 
}
\section{Methods}{\describe{
\item{ signature(X=ANY,FUN=missing) :}{lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X of class ANY.}
\item{ signature(X=FLlst,FUN=missing) :}{lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X of class FLlst.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
