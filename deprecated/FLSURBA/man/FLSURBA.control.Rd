\name{FLSURBA.control}

\alias{FLSURBA.control}
\alias{FLSURBA-class}
\alias{FLSURBA.control-class}
 
\title{ Create a new FLSURBA.control object }

\description{ 
  This function creates a new control object required by SURBA model (Survey based assessment method).
 }
 
\usage{ 

FLSURBA.control(FLSURBA = NULL, smooth=2.0)
 
}

%- maybe also 'usage' for other objects documented here.
 

\arguments{

  \item{FLSURBA}{If provided, the 'FLSURBA.control' is initialized with the corresponding 
  values of a SURBA analysis previously run.  This is useful for getting the same initial 
  parameters for successive analyses. Specifying one or more of the other arguments 
  supersedes default values, or values obtained from this FLSURBA object}
  \item{fbar}{This defines the ages over which average mortality is calculated.
This does not influence the model fit; it is only used in generating
output stock summaries.  The values refer to the position in the full
list, rather than the ages themselves; so using fbar = c(3,5) for a
stock in which the minimum age is 0 implies mean mortality calculated
over ages 2-4.
}  
  \item{refage}{In a SURBA model fit, one age effect needs to be fixed to 1.0
(to prevent indeterminancy).  This parameter specifies which age that
will be.  See comments on age position rather than true age in the notes
for fbar.}
  \item{z.spwn}{An FLQuant object that holds information on how indices need to be converted.
  By default, SURBA converts survey indices back to January 1 of
each year before using them in the model fit - this ensures inter-survey
comparability.  This date can be changed using the z.spwn parameter:
z.spwn = 0.5 means that all survey indices are converted to July 1
}
    \item{smooth}{SURBA model fits to raw survey data can lead to very noisy
estimates of mortality.  An optional smoother exists: this is a penalty
term on inter-annual variation in the fitted mortality year-effect.  The
default value is 1.0 - increasing this number will produce increasingly
smooth mortality estimates. }

}



\details{
   See \code{\link{FLSURBA}} for more details about the method and some bibliographical references.
  }



\value{
  
  An \code{FLSURBA.control} object with same slots as the arguments of the function (except \code{FLSURBA})
 }

 \author{ Laurence Kell & Coby Needle }
 \seealso{ \code{\link{FLSURBA}} }
 \examples{
  # To create a new FLSURBA.control object with default parameters:
  my.control <- FLSURBA.control()
  my.control
 
	# Same, but changing values of some parameters
 	my.control <- FLSURBA.control(smooth=2.0)

 	my.control

}
 
\keyword{ classes }



