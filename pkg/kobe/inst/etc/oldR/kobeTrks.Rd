\name{kobeTrks-methods}
\docType{methods}
\alias{kobeTrks}
\alias{kobeTrks-methods}
\alias{kobeTrks,data.frame,missing-method}
\alias{kobeTrks,numeric,numeric-method}
\title{Calculates confidence intervals for different probabiity levels for pairs of data points.}

\section{Methods}{
\describe{

\item{\code{signature(x="data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x="numeric")}}{
%%  ~~describe this method here~~
}
}}

\usage{
  kobeTrks(x,vars,prob=c(0.1,0.5,0.9),na.rm=TRUE)
}

\arguments{
  \item{x}{numeric vector representing stock or a data.frame with columns for stock and harvest}
  
  \item{vars}{character}
  
  \item{prob}{numeric vector of Trksabilities with values in [0,1].}
  
  \item{na.rm}{logical; if true, any NA and NaN's are removed from x before the quantiles are computed.}
}
               
               \description{
               \code{kobeTrks} Uses \code{kde2d} to calculate a bivariate density, normalizes the results and then calculates the contour(S) corresponding to a contained volume of points of the total volume under the surface (i.e. a two-dimensional Bayesian credible region).
               }
               \examples{
               
               #data(sims)
               #pts=subset(sims,year==2010)
               #kobeTrks(x=pts$stock, y=pts$harvest)
               
               }
 

