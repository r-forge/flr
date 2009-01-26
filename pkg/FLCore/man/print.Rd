\name{print}
\docType{methods}
\alias{print,ANY-method}
 \alias{print,FLQuant-method}
\title{Method print}
\description{
    \code{print} prints its argument and returns it invisibly (via \link{invisible}(x)).
}
\section{Generic function}{
 print(x) 
}
\section{Methods}{\describe{
\item{ signature(x=ANY) :}{Describe method}
\item{ signature(x=FLQuant) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
    a <- FLQuant(1:6, dim = c(2,3))
    for(i in 1:3) print(a[,1:i])
    for(i in 1:3) a[,1:i]
    }
\keyword{methods}
