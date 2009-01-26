\name{fmle}
\docType{methods}
\alias{fmle}
\alias{fmle,ANY,missing-method}
\alias{fmle,FLModel,ANY-method}
\alias{fmle,FLModel,FLPar-method}
\alias{fmle,FLModel-method}
\title{Method fmle}
\description{
 The \code{fmle} method is a way of passing the information in the \code{FLModel} class to the R \code{fmle} method and associated functions. 

 For a given model and log-likelihood function, the \code{fmle} method will use the \code{optim} 
 function in R to calculate the parameter vector which maximises the log-likelihood (and, hence, 
 the likelihood function) and is as such the optimum parameter value for the given problem 
 and data.

 Be advised that for non-informative of conflicting data the maximum likelihood estimate can 
 be dependent on the initial starting value and if we begin the optimiser with a poor initial 
 estimate it may converge falsely. Always try multiple start points and be assured that you 
 have found the true MLE.
}
\section{Generic function}{
 fmle(object,start) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY,start=missing) :}{Generic method.}
\item{ signature(object=FLModel,start=ANY) :}{Input object of class \code{FLModel} contains input data, logl function and function to provide initial values.}
\item{ signature(object=FLModel,start=FLPar) :}{Input object of class \code{FLModel} contains input data and logl function, but fitting is started from parameter estimates in the \code{FLPar} object provided.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
