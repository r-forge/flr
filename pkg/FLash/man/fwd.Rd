% --- Source file: man/fwd.Rd ---
\name{fwd}
\alias{fwd}

\title{fwd() A generic method for forward projection within FLR}
\description{
  \code{fwd} implements a generic method for forward projection within FLR,
it can be used with all the main FLCore objects \code{FLStock},  \code{FLStocks},
\code{FLBiol},  \code{FLBiols},  \code{FLFleet},  \code{FLFleets} and versions
will be implemented for  \code{FLIndex} and \code{FLIndices}.

Therefore \code{fwd} provides tools a common mechanism both for modelling a variety
of HCRs and for conducting MSE e.g.

\code{stk <-fwd(stk,target,sr)
stks<-fwd(stks,target,sr)
res <-fwd(biol,fleet,target,control,sr)}

The main task is in setting up approriate target \code{fwdTarget} and control \code{fwdControl}
objects, there is no need for a control for stock or stocks since with only a
single fleet and metier you can only control total effort.

\code{fwd} for \code{FLBiol(s)} and \code{FLFleet(s)} has to return two objects,
while the other calls potentially need to return things objects like the Jacobian and
diagnostics such as which control or target bit so in the future appropriate structures
for fwd to return might have to be created.

The interface allows the modelling a variety of HCRs with F/ssb/catch constraints/targets
with different priorities in just two lines e.g.

\code{target<-fwdTarget(year=2000,value=c(.40,100000,5000,120000,220000,0.65),
                        quantity=c("f", "ssb","catch","ssb","ssb","f"))
codoid.MP  <-fwd(codoid.MP, target, sr.model="mean", sr.param=25000)}

and likewise to model the MP*OM interaction

\code{TAC<-computeCatch(codoid.MP)[,"2000"]}
\code{target<-fwdTarget(year=2000,value=TAC,quantity="catch")}
\code{Codoid<-fwd(codoid,fleet,target,control,codoid.sr)[['biol']]}

We can even model compliance by specify effort constraints e.g. fleets will not
reduce F below that last year

\code{target<-fwdTarget(year=2000,value=TAC,quantity=c("catch","f"),min(NA,1.0),rel=c(NA,1999))
Codoid<-fwd(codoid,fleet,target,control,codoid.sr)[['biol']]}

or base implementation error on revenue

\code{target<-fwdTarget(year=2000,value=TAC,quantity=c("catch","revenue"),min(NA,1.0),rel=c(NA,1999))}

or value per unit effort

\code{target<-fwdTarget(year=2000,value=TAC,quantity=c("catch","vpue"),min(NA,1.0),rel=c(NA,1999))}

Therefore \code{fwd} provides a  mechanism and structures for developing HCRs and
running MSEs, which now resolves into specifying appropriate targets and controls,
without having to do any actual programming. The summary method for the control
and target objects provides a transparent method for presenting any choices made.

The target object specifies the equations to solve and the control the independent
variables (i.e. effort). This determines the size of the system of equations and
the potential control options. For example if you have 2 fleets, 2 metiers and 2
stocks you have 2*2*2=8 potential targets of about 16 different quantities (e.g. F,
biomass, catch, revenue, profit,vpue ...) but can only control effort for 2*2=4
things (i.e. fleet, metier or fleet*metier), mesh changes, seasonal/temporal
closures just resolve into another metier/fleet or unit/season/area. In addition
 some quantities only make sense by stock, metier, fleet or metier*fleet,
 e.g. ssb is by stock, catch by fleet*metier, metier or fleet, and profit by fleet.

The main tasks now resolve in creating the target and control objects for which
it is neccessary to ensure that they are set up correctly so that they are consistent
with each other and the object that being fwd() is called with.

To do this their constructors create intelligent defaults and when running fwd()
appropriate checks are made.

Once this is done mathematically and in the code all that is required is to cycle
over control when setting up f-at-age and then cycle over target when specifying
the simultaneous equations e.g. in the C++ code
}

\code{
//Control
For (i=1; i<=nfleet; i++)
  For (j=1; j<=nmetier; j++)
	f(ispp, iyr, iunit, iseason, iarea, iter) += control(ispp,ifleet, imetier)*q*sel(ispp,ifleet,imetier,…)

//Population model
N(t+1,Y+1)= N(t,Y)*exp(-F(t,Y)-M(t,Y))

//Target
//If Trgt is the target data.frame and the 3rd column is the quantity and the 4th its value

For (i=1; i<=nTrgt; i++)
   {
   Quantity = Trgt(i,3);
   switch (quantity)
   	 case FLRConst_F:
	      func[0] = Fbar(fwd,  f, ispp, ifleet, imetier, iyr, iunit, iseason, iarea, iter) - Trgt(i,4);
    	 case FLRConst_SSB:
	      func[0] = SSB(fwd,n,f, ispp, iyr, iunit, iseason, iter) - Trgt(i,4)
       case FLRConst_Catch:
	      func[0] = computeCatch(fwd,n,f, ispp, ifleet, imetier, iyr, iunit, iseason, iarea, iter) - Trgt(i,4);
	    break;
	...
      }
}

}
\usage{
fwdTarget(list(year=1998:2000,value=c(50000,51000,52000),quantity=rep("landings",3)))
}
\arguments{
  \item{object: }{either a \code{FLStock}, \code{FLStocks}, \code{FLBiol}, \code{FLBiols}}
  \item{fleet: }{either a \code{FLFleet}, \code{FLFleets}}
  \item{control: }{Control object of type fwdControl}
  \item{target: }{Target object of type fwdTarget}
  \item{sr: }{an object of type \code{FLSR}}
  \item{sr.model: }{character vector with name of stock recruit relationship}
  \item{sr.params: }{Stock recruit parameters}
  \item{sr.residuals: }{an object of type \code{FLQuant} with recruitment residuals for projection years}
  }
\details{
  \code{fwd}{allows a range HCR to be set and MSEs to be run for stocks and fisheries.}
}
\value{
  Stuff
}
\seealso{
  \code{\link{fwdControl}}, \code{\link{fwdTarget}}
}
\author{
  Laurence Kell, Finlay Scott and Iago Mosqueira
  }
\examples{
\dontrun{target<-fwdTarget(year=2000,value=c(.40,100000,5000,120000,220000,0.65),
     quantity=c("f", "ssb","catch","ssb","ssb","f"))
codoid.MP  <-fwd(codoid.MP, target, sr.model="mean", sr.param=25000)
}}

\keyword{fwd}
\keyword{fwdTarget}
\keyword{fwdControl }
