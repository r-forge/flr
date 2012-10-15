\name{kobePhaseMar}
\alias{kobePhaseMar}
\title{Plots Kobe phase plot with marginal densities.}
\usage{
  kobePhaseMar(pts,trks=NULL,mns=FALSE,size=1,
               xlab="Stock relative to Benchmark",
               ylab="F relative to Benchmark",
               maxX=2,maxY=maxX,
               col =colorRampPalette(c("orange","blue"),space="Lab"),
               col2="#CCCCCCCC")
}
\arguments{
  \item{pts}{numeric vector}

  \item{trks}{numeric vector}

  \item{mns}{logical:}

  \item{size}{numeric:}

  \item{xlab}{numeric:}

  \item{ylab}{numeric:}

  \item{maxX}{numeric:}
  
  \item{maxY}{numeric:}
  
  \item{col}{function:}
  
  \item{col2}{character:}



  
}
\description{
  \code{kobePhaseMar} Uses kde2d to calculate a bivariate density, then normalizes the results and calculates the contour corresponding to a contained volume of Phase of the total volume under the surface (i.e. a two-dimensional Bayesian credible region).
}
\examples{

####
#data(sims)
#pts=subset(sims,year=2010)
#kobePhaseMar(x=pts$stock, y=pst$harvest)

}
\seealso{
\code{\link{kobePhase}},\code{\link{kobe2sm}},\code{\link{kobeP}}
}

