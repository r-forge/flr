\name{FLSR}
\docType{class}
\alias{FLSR}
\alias{FLSR-class}
\title{Class FLSR}
\description{
  Class for stock-recruitment models, based on \code{\link{FLModel-class}}. Two extra
  \code{\link{FLQuant-class}} slots are used for input data: \code{rec} for the 
  recruitment series, and \code{ssb} for Spawning Stock Biomass or any other index of
  reproductive potential.

  All the main methods for this class are inherited from \code{\link{FLModel-class}}, 
  although the plot method is specific to FLSR, and methods exist to access the specific
  \code{FLQuant} slots, \code{rec} and \code{ssb}.

  A series of commonly-used stock-recruitment models are already available, including the
  correspodning likelihood functions and calculation of initial values. See 
  \code{\link{SRModels}} for more details and the exact formulation implemented for each
  of them.
}
\section{Slots}{\describe{
\item{name (\code{character}):}{Name of the object}
\item{desc (\code{character}):}{Description of the object}
\item{range (\code{numeric}):}{Range}
\item{rec (\code{FLQuant}):}{Recruitment series}
\item{ssb (\code{FLQuant}):}{Index of reproductive potential}
\item{fitted (\code{FLQuant}):}{Estimated values for rec}
\item{residuals (\code{FLQuant}):}{Residuals obtained from the model fit}
\item{covar (\code{FLQuants}):}{Covariates for SR model}
\item{model (\code{formula}):}{Model formula}
\item{lkhd (\code{function}):}{Likelihood function}
\item{initial (\code{function}):}{Function returning initial parameter values for the optimizer}
\item{params (\code{FLPar}):}{Estimated parameter values}
\item{logLik (\code{logLik}):}{Value of the log-likelihood}
\item{vcov (\code{array}):}{Variance-covariance matrix}
\item{details (\code{list}):}{extra information on the model fit procedure}
}}
\section{Extends}{\describe{
FLModel
FLComp
}}
\section{Methods}{\describe{
All methods are inherited, except for
\item{\link{plot}(graphics) :}{Plot an FLSR object}
\item{}{\code{ signature(x=FLQuant,y=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{FLModel}, \link{FLComp} }
\examples{
# NS Herring stock-recruitment dataset
data(nsher)

# already fitted with a Ricker SR model
summary(nsher)

plot(nsher)

# change model
model(nsher) <- bevholt()

# fit through MLE
nsher <- fmle(nsher)

plot(nsher)
}
\keyword{classes}
