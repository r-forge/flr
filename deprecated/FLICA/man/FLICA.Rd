\name{FLICA}
\alias{FLICA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Stock Assessment using Integrated Catch-at-Age Analysis (ICA) }
\description{
  Estimate the stock numbers and fishing mortality using an FLR interface to the ICA libraries
}
\usage{
FLICA(FLStock, FLIndices, FLICA.control = "missing", desc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FLStock}{ FLStock object containing catch and related data for use in the assessment.}
  \item{FLIndices}{FLIndices object containing the survey data for use in the assessment.}
  \item{FLICA.control}{ FLICA.control object, containing the settings for use in the assessment.}
  \item{desc}{ Description of the assessment run. }
}
\value{
  An FLICA object.
}
\references{Patterson, K (1997(?)) "Integrated Catch at Age Analysis" \url{http://www.ices.dk/committe/acom/wg/asoft/ICA/Doc/ICMANUAL.html}

Needle, C (2003). “Workshop Course on Fish Stock Assessment Techniques”
}
\author{ Ken Patterson. Laurie Kell}
\note{ For a example of a complete stock assessment using FLICA, see \url{http://flr-project.org/doku.php?id=documentation:tutorials:ica}}
\seealso{ \code{\link{FLICA.control}}}
\examples{

   #Load assessment objects
   data(herIIIa)
   data(herIIIa.tun)
   data(herIIIa.ctrl)
   #Perform assessment
   herIIIa.ica	<- FLICA(herIIIa, herIIIa.tun, herIIIa.ctrl)
   herIIIa <- herIIIa + herIIIa.ica
   #Show the results
   summary(herIIIa)
   herIIIa@stock.n
   herIIIa@harvest
}
\keyword{ classes }% at least one, from doc/KEYWORDS
