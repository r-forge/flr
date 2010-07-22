% $Id: AIC.Rd 44 2009-02-08 10:52:01Z imosqueira $
\name{project}
\docType{methods}
\alias{project}
\title{projection method for FLStock objects}
\description{
	A method for projecting FLStock objects.
}
\section{Usage}{
 project(stock, control, sr) 
}
\section{Method arguments}{\describe{
\item{\code{stock}   :}{an object of class \link[FLCore]{FLStock} or of one that inherits from it.}
\item{\code{control} :}{an object of class \link{projectControl} specifying the forcast parameters.}
\item{\code{sr}      :}{an object of class \link[FLCore]{FLSR} giving the stock recruit relationship.}
}}
\section{Details}{
 project conducts a deterministic projection on an FLStock object for which estimates of population abundance and harvest at age have been determined. The project method provides a simplified, pure R, version of the fwd method. The syntax and arguments to project are similar to those of fwd, however, only the basic functionality of the stock projection process is provided. Users wishing to perform more complex multi-fleet, multi-stock, area or sex disaggregated projections or to project objects other than FLStock objects should use the fwd method available from the FLash package.

The project method has three arguments. An FLStock object for which estimates of population abundance and harvest at age are available and to which the stf method has been applied (the stf method extends the FLStock object to the required number of years and fills a number of slots with the appropriate values ready for the projection). The control object specifies the target values in each year for the projection. These can be any combination of catch, landings, ssb or fishing mortality targets. Finally the method requires an FLSR object that will specify the value of future recruitments.  

In the example provided below a 5 year projection is conducted for the period 2009 to 2013. An SSB target is specified for the first year. Note that an SSB target specified for 2009 will determine the exploitation level in 2009 that will provide the target SSB at spawning time the following year. Consequently the resulting FLStock object has an SSB of 250000 t in 2010. The catch, landings and f targets all take effect in the year for which they are specified, ie. catch in 2010 is 80000 t, landings in 2011 are 60000 t and so on. Note also that the f targets correspond to the fbar value such that fbar in 2012 and 2013 are 0.4 and 0.5 respectively.  
}
\author{Rob Scott}
\seealso{\link{projectControl}, \link{stf}}
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
