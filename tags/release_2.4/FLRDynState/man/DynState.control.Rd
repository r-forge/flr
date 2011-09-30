% $ Id: $
\name{DynState.control}
\alias{DynState.control}
\title{2 species quota dynamic state variable model}
\description{
	The \code{DynState.control} function simulates decision and outcomes in a 2 species mixed  
	fisheries under Individual Tradeable Quota constraints or effort constraints. The simulation
        technique used is called dynamic state variable modelling (Clark and Mangel,2000). The input to the model is 
	defined by two DynStateInput objects.
}
\usage{
	DynState.control(Spp1Uplimit, Spp2Uplimit, EffortUplimit, SimNumber, Spp1Price, Spp2Price, EffortPrice, Spp1Increment, Spp2Increment, AddNoFishing, Spp1DiscardSteps, Spp2DiscardSteps, RIStar, ChoiceDist)
}
\arguments{
  \item{Spp1Uplimit}{The species 1 quota}
  \item{Spp2Uplimit}{The species 2 quota}
  \item{EffortUplimit}{the effort quota}
  \item{SimNumber}{The number of forward simulations}
  \item{Spp1Price}{The price of one unit of species 1}
  \item{Spp2Price}{The price of one unit of species 2}
  \item{EffortPrice}{the cost of one unit of effort}
  \item{Increments}{the number of bins for discretizing the catch of the two species in the discrete prob distribution}
  \item{AddNoFishing}{integer [0,1], indicating whether to add a single patch that reflects no fishing, catch=0, effort=0 }
  \item{Spp1DiscardSteps}{number of patches that reflect discarding species 1}
  \item{Spp2DiscardSteps}{number of patches that reflect discarding species 2}
  \item{RIStar}{integer[0,1] indicating whether the I* array shouold be returned to R, this is not possible for large problems, owing to memory restrictions}
  \item{ChoiceDist}{ouptut the treholds in the prob distribution for the landings?}
}
\details{
	This function returns an object of class DynState with the decision array and the individual
        simulations through this array with information on the effort, catch rates and exceeding of quota.
   }
\value{
    An object of class DynState.
}
\references{}
\author{Jan Jaap Poos}
\note{}
\seealso{}
\examples{}
\keyword{models} 