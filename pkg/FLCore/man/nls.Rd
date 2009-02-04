\name{nls}
\docType{methods}
\alias{nls,ANY,ANY,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{nls,FLModel,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{nls,FLModel,missing-method}
\title{Method nls}
\description{
 For a given forumla (describing a model) and data this method applies the simple non-linear 
 least squares algorithm - this calculates the parameters that minimisethe sum of squares 
 difference between the observed (data) and predicted (model) values.

 The algorithm can be sensitive to the initial values of the problem so do try different 
 start points and check they converge to the same estimates.
}
\section{Generic function}{
 nls(formula,data,start,control,algorithm,trace,subset,weights,na.action,model,lower,upper) 
}
\section{Methods}{\describe{
\item{ signature(formula=ANY,data=ANY,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) :}{Describe method}
\item{ signature(formula=FLModel,data=missing,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# An example FLSR (FLModel) object
data(nsher)

#set bevholt model
model(nsher) <- bevholt

# fit through nls
nsher <- nls(nsher)

summary(nsher)
}
\keyword{methods}
