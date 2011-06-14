\name{diagnostics}
\alias{diagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ FLICA assessment diagnostics}
\description{
  Generate a series of diagnostic plots from an FLICA stock assessment object.
}
\usage{
diagnostics(object, type=c("normal.index","all.catch"),
              par.settings=NULL, show.index=NULL, show.age=NULL, main.title=TRUE,
              plot.titles=TRUE, show.grid=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ An FLICA object resulting from a stock assessment. This is the object upon which the ensuing figures are based.}  
  \item{type}{A character vector selecting which plots to plot. See Details below. }
  \item{par.settings}{ A list of settings suitable for the \code{\link{par}} function that control the style of the figures.}
  \item{show.index}{A character vector with the names of the indices to plot diagnostics for. A value of NULL indicates all indices.}
  \item{show.age}{A character vector with the names of the ages to plot diagnostics for. A value of NULL indicates all ages.}
  \item{main.title}{Boolean. Indicates whether or not an overriding title should be plotted on each set of figures.}
  \item{plot.titles}{Boolean. Indicates whether each individual plot should have a title or not. }
  \item{show.grid}{Boolean. Places a grid on each plot.}
  \item{...}{Extra commands to be fed to the \code{plot} command.}
}
\details{
  A large number of different types of plots are available,and selected by the \code{type} option. Firstly, for each age in each survey, the following plots are possible:
  
  \item{index}{ A comparison of the time series of observed and fitted index values.}  
  \item{catchability}{A plot of the observed index value against the estimated stock numbers, with the fitted catchability model displayed for comparison.}  
  \item{time}{A time series of the residuals in the fitted index values.}  
  \item{stock}{A plot of the residuals in the fitted index values against estimated stock numbers.}  
  \item{qq}{A quantile-quantile plot of the residuals. See \code{\link{qqnorm}} for more information.}  
  \item{acf}{The partial autocorrelation function for the residuals. See \code{\link{acf}} for more information.}  

 Plots are also available for the fitted catch.

  \item{bubbles}{A bubble plot of the catch residuals.}  
  \item{selectivity}{A plot of the catch selectivity against age, with associated error bars.}  
  \item{mt.year}{A bar plot of the weighted marginal totals for each year.}  
  \item{mt.age}{A bar plot of the weighted marginal totals for each age.}  
  
 Furthermore, keywords are available to all easy selection of groups of these figures.
 
  \item{all}{Show all available plots.}  
  \item{all.catch}{Only plots relating to the catch residuals.}  
  \item{all.index}{Only plots relating to the index residuals.}  
  \item{normal.index}{The "normal" selection of plots i.e. all.index without "acf".}  
  
}

\author{ 
Based on ideas and code by Hans Bogaard during HAWG 07, and subsequently refined by Sarah Clarke. Rewritten and generalised by Mark Payne.
}

\examples{
   #Load assessment objects
   data(herIIIa)
   data(herIIIa.tun)
   data(herIIIa.ctrl)
   #Perform assessment
   herIIIa.ica  <- FLICA(herIIIa, herIIIa.tun, herIIIa.ctrl)

   #Generate diagnostics with clicking between each graph
   par(ask=TRUE)
   diagnostics(herIIIa.ica)
   diagnostics(herIIIa.ica,type="all.catch")
   diagnostics(herIIIa.ica,show.index="N20",type="normal.index")
   #Only show the catch selectivity plot on one figure
   diagnostics(herIIIa.ica,main.title=FALSE, plot.titles=FALSE, type="selectivity",par.settings=list(mfrow=c(1,1)))
   #Only show the qq-plot for the Ger AS 1-3 index at age 1
   diagnostics(herIIIa.ica,main.title=FALSE, plot.titles=FALSE, show.index="Ger AS 1-3 wr",show.age="1",type="qq",par.settings=list(mfrow=c(1,1)))
   #Write all figures to png files for adding to a report.
   png("herIIIa diagnostics - %02d.png",pointsize=16,restoreConsole=FALSE)
   diagnostics(herIIIa.ica)
   dev.off()   
}
\keyword{ classes }
