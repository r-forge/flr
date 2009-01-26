\name{sop}
\alias{sop}
\title{ Calculates the sum of products correction }
\description{
  Calculates the sum of products correction for quantities such a catch, discards, landings. 
  For example in FLStock there are slots catch.n, catch.wt and catch, catch should equal
  the products of catch.n*catch.wt summed over ages. The function returns the ratio (i.e. the 
  correction) of catch.n*catch.wt : catch, which can be used to correct either catch.n or
  catch.wt
}
\usage{
sop(stock, slot)
}
\arguments{
  \item{stock}{An FLStock object}
  \item{slot}{Name of the slot group, i.e. "catch", "landings" or "discards" for an FLStock object.}
}
\details{
  Can be used for any class or slot where there are the three FLQuant slots foo, foo.n and foo.wt
}
\value{
  Returns the ratio as an FLQuant
}
\author{FLR Team}
\examples{
data(ple4)
sop(ple4,"catch")
}
\keyword{methods}
