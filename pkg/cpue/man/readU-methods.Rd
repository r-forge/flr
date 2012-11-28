\name{readU-methods}
\docType{methods}
\alias{readU}
\alias{readU-methods}
\alias{readU,character,character-method}
\alias{readU,character,missing-method}


\title{Reads in CPUE series or other indices}
\description{
  Reads input files of the different stock assessment methods, i.e. \code{aspic} ASPIC, \code{2box} VPA-2box,\code{bsp} Bayesian Surplus Production model,
\code{mfcl} Multifan-CL, \code{ss} Stock Synthesis and \code{vpa} Lowestoft VPA Suite.
}

\section{Methods}{
\describe{
	 \item{\code{signature(file = "character", method="character")}}{}
         }}

\usage{
  readU(file, method,...)
}
\arguments{
  \item{file}{character that passes name of input file}

  \item{method}{character to species choice of assessment package, one of \code{aspic,2box,bsp,mfcl,ss,vpa}}
  
  \item{...}{other}
}


\keyword{CPUE}
\keyword{diagnostics}
\keyword{stock assessment}

\seealso{\code{\link{diagU}},~\code{\link{writeU}}}

\examples{
\dontrun{
         inp="http://iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspicb.inp"
	 cpue=readU(inp,"aspic")

	 head(cpue)
	 attributes(cpue)$smry
         }
}

