\name{FLStocks}
\docType{class}
\alias{FLStocks}
\alias{FLStocks-class}
\title{Class FLStocks}
\description{
\code{FLStocks} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLStocks} must all be of class \code{FLStock}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
\item{\link[graphics]{plot}(graphics):}{Describe method}
\item{}{\code{signature(x=FLStocks,y=missing)}}
}}
\author{The FLR Team}
\seealso{\link[graphics]{plot}, \link{FLlst}, \link[base]{list}}
\examples{}
\keyword{classes}
