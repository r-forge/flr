\name{ssn}
\docType{methods}
\alias{ssn}
\alias{ssn-methods}
\alias{ssn,FLBiol-method}
\title{Method ssn}
\description{
 Returns the Spawning Stock Numbers of \code{FLBiol} objects.
}

\section{Generic function}{
 ssn(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLBiol) :}{
 For a given \code{FLBiol} object, the spawning stock numbers are calculated 
 as follows:

 SSN = sum(n * fec * exp(-spwn * m)).

 The reason for having SSN as well as SSB is that in Leslie matrix type models it 
 is the spawning stock * numbers birthed (realised fecundity) that is the key 
 recruitment driver not SSB.
}
}}

\author{The FLR Team}
\seealso{ \link{FLComp} \link{FLStock}}
\examples{\dontrun{
 data(nsher.biol)
 data(nsher)
 n(nsher.biol) <- stock.n(nsher)
 ssn(nsher.biol)
}
}
\keyword{methods}
