% $ Id: $
\name{DynState}
\alias{DynState}

\title{2 species quota dynamic state variable model}
\description{

	The \code{DynState} function simulates decision and outcomes in a 2 species mixed  

	fisheries under Individual Tradeable Quota constraints or effort constraints. The simulation

        technique used is called dynamic state variable modelling (Clark and Mangel,2000). The input to the model is 

	defined by two DynStateInput objects.

}

\usage{	DynState(inputSpp1,inputSpp2, inputEffort, control)

}

\arguments{

  \item{inputSpp1}{A DynStateInput object holding the expected mean catch rate and Cv for species 1 (FLQuants).}

  \item{inputSpp2}{A DynStateInput object holding the expected mean catch rate and Cv for species 2 (FLQuants).}

  \item{inputEffort}{A DynStateInput object holding the effort needed (array).}

  \item{control}{A control object of class DynState.control}

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

\examples{

 library(FLRDynState)



 inputple4 <- new("DynStateInput")

 inputple4@CatchMean  <- FLQuant(rep(c(25,20,8),each=12), dimnames=list(age="all", year="all", unit="all",season=as.character(1:12), area=c("North","Mid","South")))

 inputple4@CatchSigma <- FLQuant(6, dimnames=dimnames(inputple4@CatchMean))



 inputsol4 <- new("DynStateInput")

 inputsol4@CatchMean <-  FLQuant(rep(c(1,4,6),each=12), dimnames=list(age="all", year="all", unit="all",season=as.character(1:12), area=c("North","Mid","South")))

 inputsol4@CatchSigma  <- FLQuant(2.5, dimnames=dimnames(inputsol4@CatchMean))



 effort <- array(c(9,8,7),dim=3,1)



 control <- DynState.control( Increments=20, Spp1Uplimit=20, Spp2Uplimit=30,

                             Spp1DiscardSteps=10, Spp2DiscardSteps=5, EffortUplimit=60,

                             EffortPrice=1, Spp1Price=1, Spp2Price=4,

                             ChoiceDist=1, SimNumber=100)



 z100 <- DynState(inputSpp1=inputple4, inputSpp2=inputsol4, effort, control)

}

\keyword{models} 
