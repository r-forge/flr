\name{updateU-methods}
\docType{methods}
\alias{updateU}
\alias{updateU-methods}
\alias{updateU,character,character-method}


\title{updates CPUE series or other indices to text files}
\description{
  updates input files for the different stock assessment methods, i.e. \code{aspic} ASPIC, \code{2box} VPA-2box,\code{bsp} Bayesian Surplus Production model,
\code{mfcl} Multifan-CL, \code{ss} Stock Synthesis and \code{vpa} Lowestoft VPA Suite.
}


\section{Methods}{
\describe{
	 \item{\code{signature(x, from = "character", method="character")}}{}
         }}

\usage{
  updateU(x,from,method,to,...)
}
\arguments{
  \item{x}{data frame with CPUE data}
  \item{from}{character that passes name of file to update}
  \item{method}{character to species choice of assessment package, one of \code{aspic,2box,bsp,mfcl,ss,vpa}}
  \item{to}{optional argument that allows a new file to be created}
  \item{...}{other}
}


\keyword{CPUE}
\keyword{stock assessment}

\seealso{\code{\link{readU}},~\code{\link{diagU}}~\code{\link{writeU}}}

\examples{
\dontrun{
         inp="http://iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspicb.inp"
	 cpue=updateU(inp,"aspic")

	 head(cpue)
	 attributes(cpue)$smry
         }
}

