\name{DynState.control-class}
\docType{class}
\alias{DynState.control-class}

\title{Class DynState.control}
\description{ A class to represent the controls used in the Dynamic State Variable Model}
\section{Objects from the Class}{
Objects can be created by calls to the creator of this class in the form \code{FDynState.control()}.
}
\section{Slots}{
  \describe{
    \item{\code{AddNoFishing}:}{Object of class \code{"integer"}, describing whether to add a single patch that reperesents no fishing (no effort, no catch) } 
    \item{\code{ChoiceDist}:}{Object of class \code{"integer"}, describing whether to output the probability of landings for each of the choices }
    \item{\code{EffortPrice}:}{Object of class \code{"numeric"}, price of one unit of effort }
    \item{\code{EffortUplimit}:}{Object of class \code{"numeric"}, maximum allowed amount of effort }
    \item{\code{Spp1DiscardSteps}:}{Object of class \code{"integer"}, number of discardsteps for spp1, this results in a multiplication of the number of patches }
    \item{\code{Increments}:}{Object of class \code{"integer"}, giving the number of bins used for the discretization of the probability distribution of the catches in the patches }
    \item{\code{Spp1Price}:}{Object of class \code{"numeric"}, price for unit of spp1 }
    \item{\code{Spp1Uplimit}:}{Object of class \code{"numeric"}, size of the spp1 quota }
    \item{\code{RIStar}:}{Object of class \code{"integer"}, indicating whether to output the I* array. This is not possible for very large problems because of memory constraints }
    \item{\code{SimNumber}:}{Object of class \code{"integer"}, indicating the number of simulations to do in the forward iterations }
    \item{\code{Spp2DiscardSteps}:}{Object of class \code{"integer"}, number of discardsteps for spp2, this results in a multiplication of the number of patches }
    \item{\code{Spp2Price}:}{Object of class \code{"FLQuant"}, price for unit of spp2 }
    \item{\code{Spp2Uplimit}:}{Object of class \code{"FLQuant"}, size of the spp2 quota }
  }
}
\section{Methods}{
}
\author{Jan Jaap Poos}
\examples{
}
\keyword{classes} 