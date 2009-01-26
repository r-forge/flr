\name{FLCore-package}
\alias{FLCore-package}
\docType{package}
\title{
Core package of FLR, fisheries modelling in R.
}
\description{
FLCore contains the core classes and methods for FLR, a framework for fisheries modelling and management strategy simulation in R. Developed by a team of fisheries scientists in various countries. More information can be found at http://flr-project.org/, including a development mailing list.
}
\details{
\tabular{ll}{
Package: \tab FLCore\cr
Version: \tab 1.99-111\cr
Date: \tab 2008\cr
Depends: \tab methods, R(>= 2.8.0), graphics, stats, stats4, grid, lattice\cr
License: \tab GPL 2 or above\cr
Collate: \tab FLArray.R FLQuant.R FLQuantPoint.R FLAccesors.R FLComp.R FLlst-class.R FLStock.R FLCatch.R FLQuants-methods.R FLPar.R io.FLStock.R FLBiol.R FLCohort.R FLMetier.R FLFleet.R FLIndex.R FLlst-methods.R io.FLIndices.R FLModel.R FLSR.R SRmodels.R FLtest.R coerce.R zzz.R\cr
LazyLoad: \tab Yes\cr
LazyData: \tab Yes\cr
Built: \tab R 2.8.1; i686-pc-linux-gnu; 2009-01-22 12:08:46; unix\cr
}

Index:
\preformatted{
AIC,ANY,ANY-method      Akaike information criterion (AIC) method
Arith,FLArray,FLArray-method
                        Arithmetic methods for FLQuant objects
BIC,logLik-method       Bayesian information criterion (BIC) method
E                       Method E
FLArray                 Class FLArray
FLBiol                  Class FLBiol
FLBiols                 Class FLBiols
FLBiols-methods         Method FLBiols
FLCatch                 Class FLCatch
FLCatch-accesors        Accesor and replacement methods for the FLCatch
                        class
FLCatch-methods         Method FLCatch
FLCatches               Class FLCatches
FLCatches-methods       Method FLCatches
FLCohort                Class FLCohort
FLCohort-methods        Method FLCohort
FLCohorts               Class FLCohorts
FLCohorts-methods       Method FLCohorts
FLComp                  Class FLComp
FLCore                  FLCore package of the FLR system
FLFleet                 Class FLFleet
FLFleet-methods         Method FLFleet
FLFleets                Class FLFleets
FLFleets-methods        Method FLFleets
FLIndex                 Create FLIndex objects
FLIndex-class           Class FLIndex
FLIndices               Class FLIndices
FLIndices-methods       Method FLIndices
FLMetier-class          Class FLMetier
FLMetier-methods        Method FLMetier
FLMetiers               Class FLMetiers
FLMetiers-methods       Method FLMetiers
FLModel                 Class FLModel for statistical models
FLModel-methods         Method FLModel
FLPar                   Class FLPar
FLPar-methods           Method FLPar
FLQuant                 FLQuant class for numerical data
FLQuant-methods         Create FLQuant objects
FLQuantPoint            Class FLQuantPoint
FLQuantPoint-accesors   Method lowq
FLQuantPoint-methods    Method FLQuantPoint
FLQuants                Class FLQuants
FLQuants-methods        Method FLQuants
FLSR                    Class FLSR
FLSR-methods            Method FLSR
FLStock                 Create FLStock objects
FLStock-class           Class FLStock
FLStocks                Class FLStocks
FLStocks-methods        Method FLStocks
FLlst                   Class FLlst
FLlst-methods           Method FLlst
FLtest                  FLtest functions for running simple units tests
SRModelName             Convenience function to identify an SR model by
                        its formula
SRModels                Stock-Recruitment models
[,FLArray,ANY,ANY-method
                        Extract or Replace Parts of an FLR Object
ac                      FLCore-internal
apply,ANY,missing,missing-method
                        Method apply
as.FLBiol               Old S3 coercion methods
as.data.frame,ANY,ANY,ANY-method
                        Method as.data.frame
barchart,ANY,ANY-method
                        Method barchart
bkey                    Generate key for bubbles plot
bubbles                 Bubbles plot
catchNames              Method catchNames
ccplot                  Catch-curves plot
coerce,FLlst,list-method
                        Method coerce
computeCatch            Methods to compute total catch, landings,
                        discards and stock biomass
createFLAccesors        Create accesor methods for a given class
cv                      Coefficient of Variation of FLR objects with
                        multiple iterations
dimnames<-,ANY,missing-method
                        Modify dimnames of an FLQuant
dims                    List with information on object dimensions
expand                  Trim FLR objects using named dimensions
fbar                    Calculates mean harvest rate or fishing
                        mortality
flc2flq                 Coerce FLCohort into FLQuant.
fmle                    Method fmle
harvest,FLBiol-method   Harvest calculations for FLBiol
is.FLBiol               Methods to determine the class of a given
                        object
iter                    Select or modify iterations of an FLR object
iters                   Method iters
jacknife                Jacknife resampling
lapply,ANY,missing-method
                        Method lapply
leslie                  Method for calculating Leslie matrix dynamics
                        of an FLBiol object
lowess,FLSR,missing-method
                        Method lowess
mcf                     Method mcf
mean,ANY-method         Method mean
mean.lifespan           Method for calculating mean lifespan, given the
                        natural mortality
median,ANY,missing-method
                        Method median
mergeFLStock            Merging FLStock objects
metier                  Method metier
model.frame,ANY-method
                        Method model.frame
name                    Accesor and replacement methods for complex S4
                        classes
names,ANY-method        Method names
nls,ANY,ANY,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing-method
                        Method nls
ple4                    FLCore datasets
plot,ANY,ANY-method     Method plot
predict,ANY-method      Method predict
print,ANY-method        Method print
propagate               Extend an FLQuant along the iter dimension
pv                      Population variability
qapply                  Method qapply
quant                   Method quant
quantSums               Methods to compute sums, means and vars of
                        FLQuant objects
quantTotals             Method quantTotals
quantile,ANY-method     Method quantile
r                       Method for calculating intrinsic rate of
                        increase from an FLBiol object
range-methods           Method range
readVPAFile             Input/Output of FLR objects
revenue                 Method revenue
rgamma,ANY,ANY,ANY,ANY-method
                        Method rgamma
rlnorm,ANY,ANY,ANY-method
                        Method rlnorm
rnorm,ANY,ANY,ANY-method
                        Method rnorm
rpois,ANY,ANY,ANY-method
                        Method rpois
sd,FLModel,missing-method
                        Standard deviation of an FLModel object
setPlusGroup            Method setPlusGroup
show,ANY-method         Method show
sop                     Calculates the sum of products correction
splom,ANY,ANY-method    Method splom
spr0                    Method spr0
sr                      Stock-recruitment model function
ssb                     Method ssb
ssbpurec                Method ssbpurec
ssn                     Method ssn
stripplot,ANY,ANY-method
                        Method stripplot
summary,ANY-method      Method summary
survprob                Method for calculating survival probabilties
                        given mortaloty in the FLBiol object
sweep-methods           Sweep out FLQuant Summaries
transform,ANY-method    Transform elements of a complex FLR object
trim                    Trim FLR objects using named dimensions
units,ANY-method        units attribute for FLQuant objects
update,FLModel-method   Method update
upper                   Methods upper and lower
var,ANY,missing,missing,missing-method
                        Variance of an FLPar
window,ANY-method       Extract time (year) windows of an FLR object
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
FLR Team and various contributors. Initial design by Laurence T. Kell & Philippe Grosjean.

Maintainer: FLR Team <flr-team@flr-project.org>
}
\references{
Website at \url{http://flr-project.org}

Kell L.T., Mosqueira I., Grosjean P., Fromentin J-M., Garcia D.,
		Hillary R., Jardim E., Pastoors M., Poos J.J., Scott F. & Scott R.D. 2007.
		FLR: an open-source framework for the evaluation and development of management
		strategies. ICES J. of Mar. Sci. 20: 289-290.
}
\keyword{ package }
