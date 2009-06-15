% --- Source file: man/FLash.Rd ---
\name{FLash}
\alias{FLash}
\title{Automatic Differentiation Based Tools}
\description{
  \code{FLash} implements automatic differentiation for use with FLR, based
  upon Adol-C.
}
\details{
  Flash implements automatic differentiation for FLR using the ADol-C libraries.
  These libraries are written in C++ and so are linked to FLR using the C++
  classes found in FLCoreClasses.cpp and FLCoreClasses.hpp.
  
  The main use of automatic differentation is for solving non-linear
  simultaneous equantions and for minimising non-linear functions. 
  
  Solving non-linear simultaneous equantions is used for making forward projections,
  modelling harvest control rules and for modelling the interaction between
  fishing fleets and biological populations particularly in mixed-fishery and species
  case studies. Therefore a fast flexible version of \code{fwd} is implemented in FLash
  for \code{FLStock}, \code{FLStocks}, \code{FLBiol}, \code{FLBiols}, where the later
  two also interact with \code{FLFleet} and/or \code{FLFleets}     
  
  Minimising non-linear equations can either be done for specific models or else
  as part of FLModel
  }
  
\seealso{
  \code{\link{fwd}},
  \code{\link{fwdTarget}},\code{\link{fwdControl}},
  \code{\link{FLStock}}, \code{\link{FLStock}}, \code{\link{FLBiol}}, \code{\link{FLBiols}}
  \code{\link{FLFleet}}, \code{\link{FLFleets}},
  \code{\link{FLModel}}, \code{\link{FLSR}},
}
\author{
  Laurence Kell, Finlay Scott & Iago Mosqueira
}
\examples{
\dontrun{data(ple4) 

target<-fwdTarget(list(year=1998:2000,value=.4,quantity="f"))
res   <-fwd(ple4, target, sr.model="mean", sr.param=25000)

fbar(ple4)[,as.character(1995:2000)]
fbar(res)[ ,as.character(1995:2000)]

# relative option by setting F to 1, 0.5, 0.1 times that in 1995
target[,"value"]<-c(1,.5,.1)
target[,"rel"]  <-1995
res             <-fwd(ple4, target, sr.model="mean", sr.param=25000)

fbar(res)[ ,as.character(c(1995,1998:2000))]

## Test for Catch
target<-fwdTarget(list(year    =1998:2000,value=c(10000,20000,30000),
                       quantity="catch"))
res    <-fwd(ple4, target, sr.model="mean", sr.param=25000)

computeCatch(res)[ ,as.character(1995:2000)]


## Test for SSB
# SSB targets are at end of year if harvest.spwn =0.0, otherwise in year
target<-fwdTarget(list(year=1998:2000,value=c(200000,210000,220000),quantity="ssb"))

res    <-fwd(ple4, target, sr.model="mean", sr.param=25000)
ssb(res)[ ,as.character(1995:2000)]

## Test for mixed targets
target<-fwdTarget(list(year    =rep(1995:2000),value=c(.40,100000,5000,120000,220000,0.65),
                       quantity=c("f", "ssb","catch","ssb","ssb","f")))

res    <-fwd(ple4, target, sr.model="mean", sr.param=25000)

#However if a HCR has to set the F next year depending on the biomass midway through
# next year that's a bit more difficult.

## The HCR will set F in 2003 depending on SSB in 2003
## Assessment upto and including 2001
black.bird               <-stf(ple4,nyrs=2)

# set courtship and egg laying in Autumn
black.bird@m.spwn[]      <-0.66
black.bird@harvest.spwn[]<-0.66

# assessment is in year 2002, set catch constraint in 2002 and a first guess for F in 2003
target        <-fwdTarget(year=2002:2003,value=c(85000,.5),quantity=c("catch","f"))
#target        <-fwdTarget(year=2001:2003,value=c(c(fbar(black.bird)[,"2001"]),85000,.5),quantity=c("f","catch","f"))
black.bird    <-fwd.(black.bird, target, sr.model="mean", sr.param=25000)

# HCR specifies F=0.1 if ssb<100000, F=0.5 if ssb>300000
# otherwise linear increase as SSB increases
min.ssb<-100000
max.ssb<-300000
min.f  <-0.1
max.f  <-0.5

# slope of HCR
a.    <-(max.f-min.f)/(max.ssb-min.ssb)
b.    <-min.f-a.*min.ssb

# plot of HCR
plot(c(0.0,min.ssb,max.ssb,max.ssb*2),c(min.f,min.f,max.f,max.f),type="l",ylim=c(0,max.f*1.25),xlim=c(0,max.ssb*2))

## find F through iteration
t.    <-999
i     <-0
while (abs(target[2,"value"]-t.)>10e-6 & i<50)
   {
   t.<-target[2,"value"]  ## save last value of F

   # calculate new F based on SSB last iter
   target[2,"value"]<-a.*c(ssb(black.bird)[,"2003"])+b.
   black.bird<-fwd(black.bird, target, sr.model="mean", sr.param=25000)

   # 'av a gander
   points(c(ssb(black.bird)[,"2003"]),c(target[2,"value"]),cex=1.25,pch=19,col=i)
   print(c(ssb(black.bird)[,"2003"]))
   print(c(target[2,"value"]))
   i<-i+1
   }

# F bounds
target[,"value"]<-min(max(target[,"value"],.1),.5)
black.bird      <-fwd(ple4, target, sr.model="mean", sr.param=25000)
}}

\keyword{fwd}
\keyword{fwdTarget}

\eof
