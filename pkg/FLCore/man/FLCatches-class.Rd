\name{FLCatches}
\docType{class}
\alias{FLCatches}
\alias{FLCatches-class}
\title{Class FLCatches}
\description{
\code{FLCatches} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLCatches} must all be of class \code{FLCatch}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
}
\section{Slots}{\describe{
\item{.Data (\code{list}):}{The data.}
\item{names (\code{character}):}{Names of the list elements.}
\item{desc (\code{character}):}{Description of the object.}
\item{lock (\code{logical}):}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements.}
}}
\section{Extends}{\describe{
FLlst
list
vector
}}
\section{Methods}{\describe{
\item{\link{catch}(FLCore) :}{Accessor method getting the FLCatch catch slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n}(FLCore) :}{Accessor method getting the FLCatch catch.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt}(FLCore) :}{Accessor method getting the FLCatch catch.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards}(FLCore) :}{Accessor method getting the FLCatch discards slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n}(FLCore) :}{Accessor method getting the FLCatch discards.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt}(FLCore) :}{Accessor method getting the FLCatch discards.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings}(FLCore) :}{Accessor method getting the FLCatch landings slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n}(FLCore) :}{Accessor method getting the FLCatch landings.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt}(FLCore) :}{Accessor method getting the FLCatch landings.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.q}(FLCore) :}{Accessor method getting the FLCatch catch.q slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{price}(FLCore) :}{Accessor method getting the FLCatch price slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLFleet}(FLCore) :}{Creator method of a FLFleet object from a FLCatches object}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLMetier}(FLCore) :}{Creator method of a FLMetier object from a FLCatches object}
\item{}{\code{ signature(catches=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{catch}, \link{catch.n}, \link{catch.wt}, \link{discards}, \link{discards.n}, \link{discards.wt}, \link{FLFleet}, \link{FLMetier}, \link{landings}, \link{landings.n}, \link{landings.wt}, \link{price}, \link{FLlst}, \link[base]{list}}
\examples{

# creation
catch1 <- FLCatch(name='catch1')
catch2 <- FLCatch(name='catch2')
lst.catches <- FLCatches(catch1,catch2)

# getters
catch(lst.catches) # get a FLQuants list over the species


}
\keyword{classes}
