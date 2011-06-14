\name{FLICA.out}
\alias{FLICA.out}
\alias{ica.out}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Document an FLICA stock assessment }
\description{
	Generates a pseudo "ica.out" file for use in documenting a stock assessment performed using FLICA.
}
\usage{
   FLICA.out(FLStock, FLIndices, FLICA, format="TABLE \%i.") 
   ica.out(FLStock, FLIndices, FLICA, format="TABLE \%i.") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FLStock}{ The FLStock object upon which the stock assessment is based.}  
  \item{FLIndices}{ The FLIndices object upon which the stock assessment is based. }
  \item{FLICA}{ The FLICA object containing the results of the stock assessment performed using the FLStock and FLIndices objects above. }
  \item{format}{ A C formatting string suitable for use with \code{\link{sprintf}}, characterising how each individual table in the output should be formatting. }
}
\details{
  The traditional DOS-based ICA executable produced a file called "ica.out" which contained all of the input data and run settings employed in the assessment, and which therefore served as an excellent way in which to document the assessment in, e.g. an ICES working group report. This function duplicates that functionality. 

The output is a long series of tables joined together, each documenting a different aspect of the stock assessment. The individual tables are numbered sequentially starting from 1 - the formatting of this table numbering can be controlled through the \code{format} option - see the examples for a nice illustration of how this could be used in a working group report.

The code is centered around the \code{capture.output} and \code{print} functions, and therefore the formatting of the output can be influenced in the same way as one would approach these settings. Of particular relevance are the \code{options("width")}, \code{options("scipen")} and \code{options("digits")}, which control the output width (in columns of characters), the choice of fixed vs scientific notation and the number of significant digits used, respectively. 

}
\value{
  A character vector containing the pseudo "ica.out" file. 
}

\author{ Mark Payne }

\seealso{ \code{\link{sprintf}}, \code{\link{options}} }
\examples{
   #Load assessment objects
   data(herIIIa)
   data(herIIIa.tun)
   data(herIIIa.ctrl)
   #Perform assessment
   herIIIa.ica	<- FLICA(herIIIa, herIIIa.tun, herIIIa.ctrl)
   herIIIa <- herIIIa + herIIIa.ica
   #Document assessment with FLICA.out
   options("width"=80)
   icaout <- FLICA.out(herIIIa, herIIIa.tun, herIIIa.ica, format="TABLE 3.1.\%i. WBSS HERRING.")
   #Write to file
   write(icaout,file="WBSS Herring ica.out")
}
\keyword{ classes }
