\name{VPA-methods}
\docType{methods}
\alias{VPA}
\alias{VPA-methods}
\alias{VPA,FLStock-method}
\alias{FLVPA-class}
\title{Virtual Population Analysis in FLR}
\description{
Implements Pope's Virtual Population Analysis (VPA).

The method returns an object of class \code{FLVPA} class.  This extends the \code{FLAssess} class although
the \code{FLVPA} class currently has the same slots as the \code{FLAssess} class and does not add new ones.
Objects can be created by calls of the form \code{new("FLVPA", ...)} or by calling the \code{\link{VPA}} function.

The other arguments to \code{VPA} are
\item{fratio}{ A \code{numeric} with default "missing"}
\item{fit.plusgroup}{ A \code{Boolean} with default TRUE}
\item{desc}{ A \code{character} string which appended to the desc slot of the object}

For the sake of speed, most of the calculations are carried out in C.
}
\section{Generic function}{
VPA(stock, ...)
}
\section{Methods}{\describe{
\item{ signature(object=FLStock) :}{Performs a VPA on the FLStock object.  Other arguments are fratio, fit.plusgroup and desc, which are described above.}
}}
% Could do with reference entry
\author{Laurence Kell}
\seealso{
    \code{FLSepVPA}
}
\examples{
# use the ple4 data set
data(ple4)
ple4.test <- ple4
# Remove 0s and set as 1s
catch.n(ple4.test)[catch.n(ple4.test)==0] <- 1
# Catch is odd in 1982 in pg; it's huge compared to biomass.
# This causes VPA to fail so need to reduce
catch.n(ple4.test)[,"1982"] <- 1400
# Remove harvest and stock.n values
stock.n(ple4.test)[] <- NA
harvest(ple4.test)[] <- NA
# Set Fs in final year and final ages
harvest(ple4.test)[,"2001"] <- harvest(ple4)[,"2001"]
harvest(ple4.test)[15,] <- harvest(ple4)[15,]
# Run the VPA
ple4.vpa <- VPA(ple4.test)
}
\keyword{methods}
\keyword{models}
