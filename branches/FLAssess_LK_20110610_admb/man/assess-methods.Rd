\name{assess-methods}
\docType{methods}
\alias{assess}
\alias{assess-methods}
\alias{assess,FLAssess-method}
\title{Call an stock assessment model}
\description{
    The \code{assess} methods calls the stock assessment function according to the class
    of the control object given. Stock assessment methods in FLR generally require a control
    class that contains all the options the model and its fitting algorithm provide. Classes
    are specific to each method, so see the relevant help pages for detailed information.

    The assess method allows different stock assessment procedures to be carried out in a
    simulation procedure, or in any other function, by simply altering the control class
    in the argument list.

    This is a generic method to be extended for individual stock assessment models.
}
\section{Methods}{
\describe{
\item{object = "FLAssess"}{The generic, to be extended for each specific assessment model}
}}
\keyword{methods}
