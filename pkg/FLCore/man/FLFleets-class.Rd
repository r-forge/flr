\name{FLFleets}
\docType{class}
\alias{FLFleets}
\alias{FLFleets-class}
\title{Class FLFleets}
\description{
\code{FLFleets} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLFleets} must all be of class \code{FLFleet}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
All methods are inherited.
}}
\author{The FLR Team}
\seealso{ \link{FLFleets} \link{FLlst}, \link{list}, \link{vector} }
\examples{}
\keyword{classes}
