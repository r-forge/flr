\name{writeU-methods}
\docType{methods}
\alias{writeU}
\alias{writeU-methods}
\alias{writeU,character,character-method}
\alias{writeU,data.frame,character,character-method}


\title{Writes CPUE series or other indices to text files}
\description{
  Writes input files for the different stock assessment methods, i.e. \code{aspic} ASPIC, \code{2box} VPA-2box,\code{bsp} Bayesian Surplus Production model,
\code{mfcl} Multifan-CL, \code{ss} Stock Synthesis and \code{vpa} Lowestoft VPA Suite.
}

\section{Methods}{
\describe{
	 \item{\code{signature(x="data.frame", file = "character", method="character")}}{}
         }}

\usage{
  writeU(x, file, method,...)
}
\arguments{
  \item{x}{data frame with CPUE data}
  \item{file}{character that passes name of ouput file}
  \item{method}{character to species choice of assessment package, one of \code{aspic,2box,bsp,mfcl,ss,vpa}}
  \item{...}{other}
}


\keyword{CPUE}
\keyword{diagnostics}
\keyword{stock assessment}

\seealso{\code{\link{readU}},~\code{\link{diagU}}}

\examples{
\dontrun{
         inp="http://iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspicb.inp"
	 cpue=writeU(inp,"aspic")

	 head(cpue)
	 attributes(cpue)$smry
         }
}

