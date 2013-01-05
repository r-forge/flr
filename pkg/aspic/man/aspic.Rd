\name{aspic}
\alias{aspic}
\title{ASPIC Biomass Dynamic Model Class}
\arguments{
  \item{params,}{an \code{FLPar} object with model
  parameters}
}
\value{
  biodyn object

  an \code{aspic} object
}
\description{
  A class that represents the ASPIC biomass dynamic stock
  assessment model.

  Creates an object of the \pkg{aspic} class that
  implements a biomass dynamic stock assessment model.
}
\examples{
\dontrun{aspic()}
\dontrun{bd=biodyn("logistic",FLPar(k=50000,msy=1000,b0=1))}
}

