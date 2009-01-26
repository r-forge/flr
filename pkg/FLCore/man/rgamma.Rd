\name{rgamma}
\docType{methods}
\alias{rgamma,numeric,FLQuantPoint,missing,missing-method}
\title{Method rgamma}
\description{
Random generation for the Gamma distribution with parameters 'shape' and 'scale'.  'shape' can be an FLQuantPoint in which case 'shape' and 'scale' are set as the mean(shape)^2 / var(shape).
}
\section{Generic function}{
 rgamma(n,shape,rate,scale) 
}
\section{Methods}{\describe{
\item{ signature(n=ANY,shape=ANY,rate=ANY,scale=ANY) :}{Random generation for the Gamma distribution.}
\item{ signature(n=numeric,shape=FLQuantPoint,rate=missing,scale=missing) :}{Returns an FLQuant.  The 'shape' and 'scale' arguments are taken as mean(shape)^2 / var(shape).}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(1000,mean=10,sd=2),dim=c(1,10,1,1,1,100))
flqp <- FLQuantPoint(flq)
rgamma(10,shape=flqp)
}

\keyword{methods}
