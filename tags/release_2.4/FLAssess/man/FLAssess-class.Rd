% $Id$
\name{FLAssess}
\docType{class}
\alias{FLAssess-class}
\alias{FLAssess}
\alias{FLAssess-methods}
\alias{FLAssess,missing-method}
\alias{FLAssess,FLQuant-method}
\alias{show,FLAssess-method}
\alias{units,FLAssess-method}
\alias{units<-,FLAssess,list-method}
\alias{window,FLAssess-method}
\alias{summary,FLAssess-method}
\alias{+,FLAssess,FLStock-method}
\alias{+,FLStock,FLAssess-method}
\alias{merge,FLStock,FLAssess-method}
\alias{update,FLAssess-method}
\alias{plot,FLAssess,missing-method}
\alias{harvest,FLAssess,missing-method}
\alias{index.name}
\alias{index.name<-}
\alias{index.name,FLAssess-method}
\alias{index.name<-,FLAssess,character-method}
\alias{index.hat}
\alias{index.hat<-}
\alias{index.hat,FLAssess-method}
\alias{index.hat<-,FLAssess,FLQuants-method}
\alias{index.range}
\alias{index.range<-}
\alias{index.range,FLAssess-method}
\alias{index.range<-,FLAssess,list-method}
\alias{index.res}
\alias{index.res<-}
\alias{index.res,FLAssess-method}
\alias{index.res<-,FLAssess,FLQuants-method}



\title{FLAssess class and methods}

\description{FLAssess is the basic structure for age-based stock assessment. It provides 
  a standard class for data input, diagnostic inspection and stock status estimation; 
  either for use within a working group setting or as part of a formal Management Strategy
  Evaluation (MSE). 
  
  The FLAssess class can be extended to create specific implementations of assessment methods e.g.
  FLICA, FLSURBA, FLXSA, providing a common interface for all assessmnet methods. For example,
  within ICES there are two main stock assessment methods, ICA for pelagic and XSA for demersal 
  stocks. However, differences between the methods are mainly artefacts of how they were 
  independently developed rather than methodological. By incorporating such methods in a common 
  class this problem will hopefully be avoided in the future. 

  FLAssess also incorporates methods for performing virtual population analysis (VPA) and stock 
  projection.
  
  In common with other FLR classes, all of the slots of the FLAssess class have accessor and replacement methods.
  Users are encouraged to use these instead of the @ slot accessor.
}
	
\section{Objects from the Class}{
  Objects can be created by calls to the methods VPA or SepVPA.
  They also can be created by calls of the form
  \code{new("FLAssess", ...)}.
}

\section{Slots}{
  \describe{
    \item{name:}{A character string give name of stock.}
    \item{desc:}{Whatever you want.}
    \item{range:}{A named vector specifying the range of the object.}
    \item{catch.n:}{An FLQuant that contains estimated catch numbers-at-age.}
    \item{stock.n:}{An FLQuant that contains estimated stock numbers-at-age.}
    \item{harvest:}{An FLQuant that contains estimated fishing mortlaity-at-age.}
    \item{index.name:}{A vector containing names for the FLIndexs}
    \item{index.range:}{A list containing ranges for the FLIndexs}
    \item{index:}{A list of FLQuants, corresponding to the index values used when fitting.}
    \item{index.hat:}{A list of FLQuants, corresponding to the fitted index values.}
    \item{index.res:}{A list of FLQuants, corresponding to the index residuals.}
    \item{index.var:}{A list of FLQuants, corresponding to the variances of index values in fitting.}    
  }
}

\section{Methods}{\describe{

%Summarise contents:
    \item{\link{summary}:}{ Returns a list, with a vector for each dimension in the object, that contains the levels of each dimension.}
    \item{\link{show}:}{ Prints the given object.}
    \item{\link{plot}:}{Plots stock.n against index}

    \item{+:}{Updates a FLStock with stock.n and harvest estimates from an FLAssess object}
    \item{merge:}{Updates a FLStock with stock.n and harvest estimates from an FLAssess object}
    \item{\link{update}:}{Updates a FLStock with stock.n, catch.n & harvest estimates}
%Stock Assessment methods:
    \item{\link{SepVPA}:}{Separable VPA}
    \item{\link{VPA}:}{Virtual Population Analysis}
%   \item{\link{ASPM}:}{Age Structured Production Model}
}}

\author{FLR Team}
\seealso{
    \code{FLIndices-class},
    \code{FLStock-class}
}
\examples{
   # see the Methods help pages for specific examples  

}
\keyword{classes}
\keyword{methods}
