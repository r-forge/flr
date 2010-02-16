\name{FLBREM-class}
\docType{class}
\alias{FLBREM-class}
\alias{plot,FLBREM,missing-method}
\alias{summary,FLBREM-method}
\alias{name,FLBREM-method}
\alias{name<-,FLBREM,character-method}
\alias{desc,FLBREM-method}
\alias{desc<-,FLBREM,character-method}

\title{Class FLBREM: fishery independent stock assessment method}
\description{
    The \code{FLBREM} class models catch per unit effort time series to recreate
    trends in recruitment, adult population and population growth
    
    Slots store the relevant data, e.g. population by quant. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FLBREM",...)} or through the creator function \code{FLBREM()}.
}

\section{Creator Function}{
  The function \code{FLBREM()} creates a new \code{FLBREM} object. To initialise the object,
	an FLIndexSurvey or FLIndices (containing FLIndexSurvey objects) should be provided representing
  the adult and recruit surveys. Additionally an FLBREM.control object can be provided to change
  the method options.
}

\section{Slots}{
  \describe{
 
    \item{\code{control}:}{ The \code{FLBREM.control} object used to create the obect}
   	\item{\code{call}:}{ The call used to create the object}
   	\item{\code{desc}:}{ Description }
    \item{\code{g}:}{ An \code{FLQuant} representing the total annual population change}
    \item{\code{stock.n}:}{ An \code{FLQuant} representing the population, recruits and adults}      
		\item{\code{r.mu}:}{ The average recruitment}
		\item{\code{r.cv}:}{ The cv of recruitement on the log normal scale}
		\item{\code{r.index.q}:}{Catchability of recruit survey}
		\item{\code{r.index.cv}:}{CV of recruit survey errors on log scale}
		\item{\code{adult.q}:}{ Catchability of adult survey}
		\item{\code{adult.cv}:}{ CV of adult survey errors on log scale}
		\item{\code{g0}:}{	Log growth in first year}
		\item{\code{g.sigma}:}{SD of g deviatess} 
		\item{\code{stock0}:}{Biomass in first year}
    \item{\code{obj.val}:}{Objective value} 
}}

\section{Methods}{
  \describe{
    \item{is.FLBREM}{\code{signature(x = "FLBREM")}: ... }
    \item{summary}{\code{signature(object = "FLBREM")}: ... }
    \item{plot}{\code{signature(x = "FLBREM", y = "missing")}: ... }
    }
}
\author{Laurence Kell, Verena Trenkle}
\seealso{
  \code{\link{FLIndexSurvey}}
}
\examples{

  ## get example data sets
  data(rec)
  data(adult)
  
  ## run the model
  FLBREM(rec,adult,control=FLBREM.control(r.index.q=1,r.index.cv=0.1,adult.q=1,adult.cv=0.2))
}

\keyword{classes}
\keyword{manip}