\name{no.discards-methods}
\docType{methods}
\alias{no.discards}
\alias{no.discards-methods}
\alias{no.discards,FLStock-method}
\title{Sets catch data if no discards}
\description{
This method sets the slots \code{discards.n} and \code{discards.wt} to 0, and
sets the slots \code{catch}, \code{catch.n} and \code{catch.wt} to their \code{landings}
equivalents.
%obj@discards   <-FLQuant(0,dimnames=dimnames(obj@discards))
%      obj@discards.n <-FLQuant(0,dimnames=dimnames(obj@discards.n))
%      obj@discards.wt<-FLQuant(0,dimnames=dimnames(obj@discards.wt))
%      obj@catch      <-obj@landings
%      obj@catch.n    <-obj@landings.n
%      obj@catch.wt   <-obj@landings.wt
}
\section{Methods}{
\describe{

\item{\code{no.discards<-(obj)}}{Takes in an object of type \code{FLStock} and carries out
the above actions. Returns an object of type \code{FLStock}}
}}
\keyword{methods}
\keyword{manip}
