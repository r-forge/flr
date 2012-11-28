\name{diagU-methods}
\docType{methods}
\alias{diagU}
\alias{diagU-methods}
\alias{diagU,character,character-method}


\title{Reads in residuals from CPUE series or other indices fitted within a stock assessment model.}
\description{
  Reads CPUE residuals from the files output by different stock assessment methods, i.e. \code{aspic} ASPIC, \code{2box} VPA-2box,\code{bsp} Bayesian Surplus Production model,
\code{mfcl} Multifan-CL and \code{ss} Stock Synthesis. These can then be used to explore the fit of the stock assessment model to the data.
}
\section{Methods}{
\describe{
	 \item{\code{signature(file = "character", method="character")}}{}
         }}

\usage{
  diagU(file, method,...)
}
\arguments{
  \item{file}{character that passes name of input file}

  \item{method}{character to species choice of assessment package, one of \code{aspic,2box,bsp,mfcl,ss,vpa}}
  
  \item{...}{other}
}


\keyword{CPUE}

\seealso{\code{\link{readU}},~\code{\link{writeU}}}

\examples{
\dontrun{
         inp="http://iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspicb.inp"
	 cpue=diagU(inp,"aspic")

	 head(cpue)
	 attributes(cpue)$smry
         }
}

