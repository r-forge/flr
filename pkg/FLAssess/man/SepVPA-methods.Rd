\name{SepVPA-methods}
\docType{methods}
\alias{SepVPA}
\alias{SepVPA-methods}
\alias{SepVPA,FLStock-method}
\alias{assess,FLSepVPA.control-method}
\title{Separable Virtual Population Analysis in FLR}
\description{
Performs Separable Virtual Population Analysis. Methods for function
\code{SepVPA} in package \pkg{in Package `FLAssess'}. Requires an object of class
\code{\link{FLSepVPA.control}} to be created.
}
\section{Methods}{
\describe{

\item{\code{SepVPA<-function(stock, control=FLSepVPA.control(), ref.harvest="missing", fratio="missing", fit.plusgroup=TRUE, desc="",...)}}{, where
                      \code{stock} is of type \code{FLStock}, \code{control} is of type \code{FLSepVPA.control()}, \code{ref.harvest}
                      and \code{fratio} are numeric and \code{fit.plusgroup} is Boolean.}
}}

\examples{
# Example based on ple4 dataset
data(ple4)
# Set up stock with correct dimensions
my.stock <- FLStock(catch.n(ple4))
my.stock@range["plusgroup"] <- 15
#load catch data and mortality
my.stock@catch.n <- ple4@catch.n
my.stock@catch.n[my.stock@catch.n==0] <- 1
my.stock@m <- ple4@m
my.control <- FLSepVPA.control(sep.age = 5)
# Set up in final year
my.stock@stock.n[,"2001"] <- ple4@stock.n[,"2001"]
# Run SepVPA
my.stock.SepVPA <- SepVPA(my.stock, my.control, fit.plusgroup=TRUE)
}

\keyword{methods}
\keyword{models}
