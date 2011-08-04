\name{Sim-class}
\docType{class}
\alias{Sim-class}

\title{Class Sim}
\description{ A class to represent the outputs of the forward iterations from a dynamic state variable model for a mixed fishery under quota constraints. This is part of the returned object from a model run }
\section{Slots}{
  \describe{
    \item{\code{Choice}:}{Object of class \code{"array"}, indicating which option is chosen. rows are forward simulation number, colums are timesteps }
    \item{\code{Derivative}:}{Object of class \code{"array"}, indicating what is the patch that is chosen}
    \item{\code{Effort}:}{Object of class \code{"array"}, indicating how much effort is used in this time step }
    \item{\code{Exceed}:}{Object of class \code{"array"}, whether one of the quota is exceeded }
    \item{\code{Spp1Discards}:}{Object of class \code{"array"}, the amount of spp1 discards in this time step  }
    \item{\code{Spp1Hold}:}{Object of class \code{"array"}, total (annual) spp1 landings after this timestep }
    \item{\code{Spp1Landings}:}{Object of class \code{"array"}, spp1 landings in this time step }
    \item{\code{Spp1Rand}:}{Object of class \code{"array"}, random number drawn, in the forward simulation on which the spp1 landings are based }
    \item{\code{Spp2Discards}:}{Object of class \code{"array"}, the amount of spp2 discards in this time step}
    \item{\code{Spp2Hold}:}{Object of class \code{"array"}, total (annual) spp2 landings after this timestep }
    \item{\code{Spp2Landings}:}{Object of class \code{"array"}, spp2 landings in this time step }
    \item{\code{Spp2Rand}:}{Object of class \code{"array"}, random number drawn, in the forward simulation on which the spp2 landings are based }
  }
}
\section{Methods}{
}
\author{Jan Jaap Poos}
\examples{
}
\keyword{classes} 