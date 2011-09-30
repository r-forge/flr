\name{DynState-class}
\docType{class}
\alias{DynState-class}

\title{Class DynState}
\description{ A class to represent the different results from the forward and backward calculations of a Dynamic State Variable Model}
\section{Objects from the Class}{
This object is created by the RDynState function
}
\section{Slots}{
  \describe{
   
    \item{\code{ChoiceDist}:}{Object of class \code{"array"}, giving the probability distribution of the landings for the different options }
    \item{\code{control}:}{Object of class \code{"DynState.control"} being the control object that was used to run the model}
    \item{\code{RIStar}:}{Object of class \code{"array"}, the I* array that result from the backward calculation  }
    \item{\code{Sim}:}{Object of class \code{"Sim"}, the result of the forward iterations }

  }
}
\section{Methods}{
}
\author{Jan Jaap Poos}
\examples{
}
\keyword{classes} 