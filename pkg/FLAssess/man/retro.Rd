% $Id: retro.Rd,v 1.12 2009/02/10 16:28:17 fas00 Exp $
\name{retro-methods}
\docType{methods}
\alias{retro}
\alias{retro-methods}
\alias{retro,FLStock,FLIndex,ANY,numeric-method}
\alias{retro,FLStock,FLIndices,ANY,numeric-method}
\title{ Retrospective stock assessment in FLR }
\description{
Performs a retrospective stock assessment for the desired years using the
stock assessment method specified in the control object (see \code{FLAssess}
and extended classes)}
\section{Generic function}{
retro(stock,indices,control,retro,...)
}
\section{Methods}{\describe{
\item{ signature(stock=FLStock,indices=FLIndex,control=ANY,retro=numeric) :}{Takes a single index as an \code{FLIndex} object.}
\item{ signature(stock=FLStock,indices=FLIndices,control=ANY,retro=numeric) :}{Takes an \code{FLIndices} object for multiple indices.}
}}



%\arguments{
%  \item{FLStock}{ An object of type \code{FLStock} that contains information
%  (catch data, natural mortality etc.) on the stock that is to be assessed}
%  \item{FLIndices}{An object of type \code{FLIndices} that contains the tuning
%  data}
%  \item{control}{A control object of the desired extended \code{FLAssess}
%  assesment method class, e.g. \code{FLXSA.control}}
%  \item{year.range}{Numeric vector of years to perform the assessment}
%  \item{retro}{An integer that specifies the number of retrospective years.
%  Default value = 0.  Only used if year.range is not specified.}
%
%}
\details{
The type of assessment method used is determined by the control object, e.g.
\code{FLXSA.control}, \code{FLICA.control} etc.  In this way retrospective
analysis using different assessment methods can be easily carried out on the
same stock and tuning data by using different control classes.

The argument 'year.range' is a numeric vector of years for which the assessment
is to be performed.  If this is not specified the integer ''retro'' is used
(default value = 0).  If retro = 0 the assessment is run for final year only.
If retro = 1, the assessment is run for the penultimate and final year and so on.

The results of the restrospective analysis can be plotted using the
generic FLStocks \code{plot()} method.

NOTE: This function supercedes the previous \code{retro()} function which returned
an object of type \code{FLAssess.retro}.  The \code{FLAssess.retro} class is now
obsolete and will not be present in future releases.  Users are advised to
update their scripts to use this new function.  The original function is still in
the \code{FLAssess} package with the name \code{retro.old()}.
}
\value{
Returns an object of type \code{FLStocks}.  Each component \code{FLStock} object
contains the result of each of the retrospective assessments.
}

\author{Laurence Kell}
\seealso{\code{\link{FLAssess-class}}, \code{FLICA}, \code{FLXSA}}
\keyword{models}
