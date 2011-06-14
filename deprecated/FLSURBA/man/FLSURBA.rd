\name{FLSURBA}
\alias{FLSURBA}
\alias{FLSURBA-class}
\alias{FLSURBA.control-class}
\title{Survey Based Assessment Method}
\description{  The basis of SURBA is a simple survey-based separable model of mortality. 
 
}
\usage{
    FLSURBA(stock, index, bindex, control)
}
\arguments{
    \item{stock}{ FLStock object, only uses m, stock.wt and mat slots}
    \item{index}{ FLIndices object that contains FLIndexSurvey objects holding numbers-at-age}
    \item{bindex}{ Optional FLIndices object that contains FLIndexSurvey objects holding biomass}
    \item{control}{ FLSURBA.control object, if not supplied default will be used}
    \item{output}{ returns an FLSURBA object }
}
\details{ The separable model used in SURBA assumes that total mortality Z(a,y) in age a and year y can
be expressed as 

Z(a,y) = s(a)*f(y) 

where s(a) and f(y) are respectively the age and year effects of mortality. Note that this differs from the usual
assumption in that total mortality Z is the quantity of interest, rather than fishing mortality F.

The parameters to be estimated when fitting the model are Age effects, year effects, and cohort effects. 
SURBA assumes that at least one age-structured survey index is available: biomass indices are optional. If
present, it is assumed that biomass indices are measured at spawning time, following the convention used
in such models as ICA (Patterson and Melvin 1996).

For the fitting abundance indices (age-structured and biomass) are mean-standardised so that the mean of each index over all ages and years is 1.0.
The way SURBA handles age-structured or biomass indices is described in the manual included in this package.
}
\value{
    \item{"desc"}{it could include a description of the run}
    \item{"call"}{provides the history on how FLSURBA was called}
    \item{"control"}{ FLSURBA.control object }
    \item{"error"}{ error flag }
    \item{"stock.n"}{ Abundance }
    \item{"z"}{	 Total mortality }
    \item{"z.bar"}{ Mean z }
    \item{"z.var"}{ Variance of total mortality }
    \item{"z.varbar"}{	Variance of mean Z }
    \item{"r.var"}{ Variance of recruitment }
    \item{"index.res"}{	Residuals for age-structured indices }
    \item{"bindex.res"}{ Residuals for biomass indices }
}
\seealso{FLStock, FLIndexSurvey, FLIndices, FLSURBA.control
}
\examples{The following code needs to be run prior calling FLSURBA. In the following example
x is an FLStock object and x.idx is an FLIndices object.
 
 # Specify indices

for (i in 1:length(x.idx))
{
	# Set type of indices
	x.idx[[i]]@type <- "numbers"
	# Initialise plus-groups for indices
	x.idx[[i]]@range["plusgroup"] <- NA
	# Define SSQ weightings for indices
	x.idx[[i]]@index.var[] <- 1.0
	# Define catchabilities for indices
	x.idx[[i]]@index.q[] <- 1.0
	# Mean-standardise indices (accounting for plus-group)
	x.idx[[i]]@index[] <- x.idx[[i]]@index[] /
mean(x.idx[[i]]@index[])
}

min.x.age <- min(unlist(lapply(x.idx,
function(wk.x){wk.x@range["min"]})))
max.x.age <- max(unlist(lapply(x.idx,
function(wk.x){wk.x@range["max"]})))
min.x.year <- min(unlist(lapply(x.idx,
function(wk.x){wk.x@range["minyear"]})))
max.x.year <- max(unlist(lapply(x.idx,
function(wk.x){wk.x@range["maxyear"]})))

# Trim catch dataset
x <- window(trim(x, age = min.x.age:max.x.age), start = min.x.year, end
= max.x.year)

# Fill in final-year values for catch dataset (three-year means)
x@stock.wt[,ch(max.x.year)] <- apply(x@stock.wt[,ch((max.x.year -
3):(max.x.year - 1))], 1, mean)
x@mat[,ch(max.x.year)] <- apply(x@mat[,ch((max.x.year - 3):(max.x.year -
1))], 1, mean)
x@m[,ch(max.x.year)] <- apply(x@m[,ch((max.x.year - 3):(max.x.year -
1))], 1, mean)
x@catch.wt[,ch(max.x.year)] <- apply(x@catch.wt[,ch((max.x.year -
3):(max.x.year - 1))], 1, mean)
x@landings.wt[,ch(max.x.year)] <- apply(x@landings.wt[,ch((max.x.year -
3):(max.x.year - 1))], 1, mean)
x@discards.wt[,ch(max.x.year)] <- apply(x@discards.wt[,ch((max.x.year -
3):(max.x.year - 1))], 1, mean)
x@harvest.spwn[,ch(max.x.year)] <- apply(x@harvest.spwn[,ch((max.x.year
- 3):(max.x.year - 1))], 1, mean)
x@m.spwn[,ch(max.x.year)] <- apply(x@m.spwn[,ch((max.x.year -
3):(max.x.year - 1))], 1, mean)

x@catch <- x@landings
x@catch.n <- x@landings.n
x@catch.wt <- x@landings.wt

x.surba.control <- FLSURBA.control(smooth = 1.0, fbar = as.integer(c(2,4)), 
	refage = as.integer(3))
x.surba <- FLSURBA(x, x.idx, x.surba.control)
  
}
\references{see manual included
}
\notes{ Note that FLSURBA needs the NAG libraries to run
}
\author{Laurence Kell, Coby Needle}
\keyword{classes}
\keyword{methods}
