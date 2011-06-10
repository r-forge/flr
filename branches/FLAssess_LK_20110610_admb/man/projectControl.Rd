% $Id: projectControl.Rd $
\name{projectControl}
\docType{class}
\alias{projectControl}
\alias{projectControl-class}
\alias{projectControl-methods}
\alias{projectControl,data.frame-method}
\alias{projectControl,missing-method}

\title{control object for the project method for FLStock objects}
\description{
	A control object for the project method for projecting FLStock objects.
}
\section{Usage}{
 projectControl(dataframe) 
}
\section{Method arguments}{\describe{
\item{\code{dataframe} :}{a dataframe containing the targets and corresponding years.}
}}
\section{Details}{
A control object is required to specify the target values in each year of the projection. The control object for the project method is similar (though simplified) to that used by fwd. 

In the example provided below a 5 year projection is conducted for the period 2009 to 2013. An SSB target is specified for the first year. Note that an SSB target specified for 2009 will determine the exploitation level in 2009 that will provide the target SSB at spawning time the following year. Consequently the resulting FLStock object has an SSB of 250000 t in 2010. The catch, landings and f targets all take effect in the year for which they are specified, ie. catch in 2010 is 80000 t, landings in 2011 are 60000 t and so on. Note also that the f targets correspond to the fbar value such that fbar in 2012 and 2013 are 0.4 and 0.5 respectively.  
}
\author{Rob Scott}
\seealso{\link{project}, \link{stf}}
\examples{
data(ple4)
ple4.stf <- stf(ple4, 5)
ple4.sr  <- sr(as.FLSR(ple4,model=geomean))
ctrl     <- projectControl(data.frame(year=2009:2013, 
                                      val=c(250000, 80000, 60000, 0.4, 0.5), 
                                      quantity=c('ssb','catch','landings','f','f')))
res      <- project(ple4.stf, ctrl, ple4.sr)

}
\keyword{methods}
