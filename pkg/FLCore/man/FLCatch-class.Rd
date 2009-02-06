\name{FLCatch}
\docType{class}
\alias{FLCatch}
\alias{FLCatch-class}
\title{Class FLCatch}
\description{
An internal class for \code{FLMetier} objects that stores stock or species specific information
on landings, discards and catches, catchability and price.

Catches is the sums of landings and discards.

Landings, discards and catches are described with four slots each, one intented to deal
with age (or other quant)-aggregated total estimate and three intented to deal with age
(or other quant)-disaggregated information (numbers at age, mean weight at age and selectivity at age).

}
\section{Slots}{\describe{
\item{catch (\code{FLQuant}):}{Total catches in weight}
\item{catch.n (\code{FLQuant}):}{Numbers at quant in catches}
\item{catch.wt (\code{FLQuant}):}{Mean weight at quant in catches}
\item{catch.sel (\code{FLQuant}):}{Selectivity at quant in the catches}
\item{catch.q (\code{FLQuant}):}{Catchability in the catches. Can be quant-disagggregated or not}
\item{landings (\code{FLQuant}):}{Total landings in weight}
\item{landings.n (\code{FLQuant}):}{Numbers at quant in landings}
\item{landings.wt (\code{FLQuant}):}{Mean weight at quant in catches}
\item{landings.sel (\code{FLQuant}):}{Selectivity at quant in the catches}
\item{discards (\code{FLQuant}):}{Total discards in weight}
\item{discards.n (\code{FLQuant}):}{Numbers at quant in catches}
\item{discards.wt (\code{FLQuant}):}{Mean weight at quant in discards}
\item{discards.sel (\code{FLQuant}):}{Selectivity at quant in the discards}
\item{price (\code{FLQuant}):}{mean price. Can be quant-disagggregated or not }
\item{name (\code{character}):}{Generic name slot inherited from FLComp class}
\item{desc (\code{character}):}{Generic description slot inherited from FLComp class}
\item{range (\code{numeric}):}{Generic range slot inherited from FLComp class indicating
 min quant, max quant, plusgroup quant,minyear and maxyear }
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link[graphics]{as.data.frame}(base) :}{coerces the FLCatch object to a data.frame. Slot 
  names are passed in the slot column. The name, desc and range slots is passed as an
  attribute of the data.frame}
\item{}{\code{ signature(x=FLCatch,row.names=missing,optional=missing) }}
\item{\link{catch}(FLCore) :}{accessor method for the catch slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch<-}(FLCore) :}{replacement method for the catch slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.q}(FLCore) :}{accessor method for the catch.q slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.q<-}(FLCore) :}{replacement method for the catch.q slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.n}(FLCore) :}{accessor method for the catch.n slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n<-}(FLCore) :}{replacement method for the catch.n slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.sel}(FLCore) :}{accessor method for the catch.sel slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.sel<-}(FLCore) :}{replacement method for the catch.sel slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.wt}(FLCore) :}{accessor method for the catch.wt slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt<-}(FLCore) :}{replacement method for the catch.wt slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards}(FLCore) :}{accessor method for the discards slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards<-}(FLCore) :}{replacement method for the discards slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.n}(FLCore) :}{accessor method for the discards.n slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n<-}(FLCore) :}{replacement method for the discards.n slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.sel}(FLCore) :}{accessor method for the discards.sel slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.sel<-}(FLCore) :}{replacement method for the discards.sel slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.wt}(FLCore) :}{accessor method for the discards.wt slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt<-}(FLCore) :}{replacement method for the discards.wt slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings}(FLCore) :}{accessor method for the landings slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings<-}(FLCore) :}{replacement method for the landings slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.n}(FLCore) :}{accessor method for the landings.n slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n<-}(FLCore) :}{replacement method for the landings.n slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.sel}(FLCore) :}{accessor method for the landings.sel slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.sel<-}(FLCore) :}{replacement method for the landings.sel slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.wt}(FLCore) :}{accessor method for the landings.wt slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt<-}(FLCore) :}{replacement method for the landings.wt slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{price}(FLCore) :}{accessor method for the price slot}
\item{}{\code{ signature(object=missing) }}
\item{\link{price<-}(FLCore) :}{replacement method for the price slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{FLFleet}(FLCore) :}{creates a FLFleet object with one single metier from a FLCatch object.
All other FLFleet and FLMetier slots are set to NA.  }
\item{}{\code{ signature(object=missing) }}
\item{\link{FLMetier}(FLCore) :}{creates a FLMetier object from a FLCatch object. Effshare and
vcost slots are set to NA.}
\item{}{\code{ signature(catches=missing) }}
\item{\link[methods]{coerce}(methods) :}{coerces a FLStock object into a FLCatch object. This method is called
by typing as(x,"FLCatch"). In that case, catch.sel is calculated as harvest(x)/max(harvest(x)),
catch.q is set to 1 and landings.sel and discards.sel are calculated proportionally to numbers}
\item{}{\code{ signature(from=NULL,to=FLCatch,strict=missing) }}
\item{\link{computeCatch}(FLCore) :}{computes the total catch from the quant-structured
	values in numbers and weight per individual.}
\item{}{\code{ signature(object=missing) }}
\item{\link{computeDiscards}(FLCore) :}{computes the total discards from the quant-structured
	values in numbers and weight per individual.}
\item{}{\code{ signature(object=missing) }}
\item{\link{computeLandings}(FLCore) :}{computes the total landings from the quant-structured
	values in numbers and weight per individual.}
\item{}{\code{ signature(object=missing) }}

}}

\author{The FLR Team}
\seealso{ \link[base]{as.data.frame}, \link{catch}, \link{catch<-}, \link{catch.q}, \link{catch.q<-},
\link{catch.n}, \link{catch.n<-}, \link{catch.sel}, \link{catch.sel<-}, \link{catch.wt},
\link{catch.wt<-}, \link[methods]{coerce}, \link{computeCatch}, \link{computeDiscards},
\link{computeLandings}, \link{discards}, \link{discards<-}, \link{discards.n}, \link{discards.n<-},
\link{discards.sel}, \link{discards.sel<-}, \link{discards.wt}, \link{discards.wt<-},
\link{FLFleet}, \link{FLMetier}, \link{landings}, \link{landings<-}, \link{landings.n},
\link{landings.n<-}, \link{landings.sel}, \link{landings.sel<-}, \link{landings.wt}, \link{landings.wt<-},
\link{price}, \link{price<-}, \link{FLComp} }
\examples{
data(ple4)
mycatch<-as(ple4,"FLCatch")

landings(mycatch)<-computeLandings(mycatch)
landings(mycatch)

}
\keyword{classes}
