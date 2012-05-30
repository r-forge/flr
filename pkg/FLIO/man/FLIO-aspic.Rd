\name{aspic-IO methods}
\alias{readAspic}
\alias{aspicProj}
\alias{aspicRuns}
\title{Input of aspic files}
\description{
  These functions read the various aspic files and create R and FLR objects that represent historic and projected 
  stock status, reference points and management advice.
  
  
ASPIC has proection and assessment modes, both of which have control, input, output and summary files. These  files are in different formats for reading by different software. Here we only deal with the files that can be read easily by R, i.e.

\tabular{ll}{
          .inp   \tab Input file with data, starting guesses, and run settings. \cr
          .bio   \tab Estimated B and F trajectory for each bootstrap trial     \cr
          .prb   \tab As .bio but with projection results.                      \cr
          .rdat  \tab Inputs and estimates specially formatted for R.           \cr
          .det   \tab Estimates from each bootstrap trial                       \cr
          .prn   \tab summary                                                  \cr
          }
          
    Example text files can be found in the inst/inputs/aspic directory of the package

Providing a file to readAspic determines what is returned.

\tabular{ll}{
          .inp   \tab                                                           \cr
          .bio   \tab                                                           \cr
          .prb   \tab                                                           \cr
          .rdat  \tab                                                           \cr
          .det   \tab                                                           \cr
          .prn   \tab                                                          \cr
          }         

  }
\usage{
  readAspic(x)
  aspicRuns(x, scen)
  aspicProj(x, scen)
}
\arguments{
   \item{x}{ Name of aspic file, for aspicRuns and AspicProj this is the base name with no extension.}
   \item{scen}{ a vector or data.frame with the scenarios run, i.e. run number or TAC}
}
\details{
  These functions are used for reading data from aspic files, further information can be obtained from \url{http://www.mhprager.com/aspic5man.pdf}.
  
}
\value{
  An object of class \code{FLQuants}, \code{FLStock}, \code{FLCPUE} or \code{aspic}.
}
\seealso{
\link{FLBioDym}, \link{aspic}.
}
\examples{
\dontrun{
## reading in 

}

}
\references{  
 Prager, M. H. 1994. A suite of extensions to a nonequilibrium surplus–production model. Fishery Bulletin 92: 374–389.

}
\author{The FLR Team}
\keyword{IO}
