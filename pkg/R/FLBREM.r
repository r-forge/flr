### FLR core version 1.0 ###
### FLBREM
### This file contains code just for the core class FLBREM

### classes ####################################################################

### FLBREM.control #############################################################
validFLBREM.control <- function(object){
	
	check.control<-function(slot,object){
		res<-as.logical(TRUE)
		x  <-slot(object, slot)

		if (!all(sort(names(x))==sort(c("min","max","phase")))) {
			warning(paste("control option", slot, "does not have elements 'min', 'max' and 'phase'"))
			res<-as.logical(FALSE)
			}
		
		if (x["min"] > x["max"]){
			warning(paste("min > max in control option", slot))
			res<-as.logical(FALSE)
			}

        return(res)
		}

	res<-(check.control("r.cv",		     object)  &
	      check.control("r.index.q",   object)  &
	      check.control("r.index.cv",  object)  &
	      check.control("adult.q",	   object)  &
	      check.control("adult.cv",    object)  &
	      check.control("g0",		       object)  &
	      check.control("g.sigma",	   object)  &
	      check.control("stock0",	     object))

	return(res)
	}

setClass("FLBREM.control",
	representation(
		range		   ="numeric",		## average recruitment
		r.mu		   ="numeric",		## average recruitment
		r.cv		   ="numeric",		## cv recruitement logNormal scale
		r.index.q	 ="numeric",		## Catchability recruits (survey specific)
		r.index.cv ="numeric",		## CV of recruit survey errors on log scale
		adult.q		 ="numeric",		## Catchability adults (survey specific)
		adult.cv	 ="numeric",		## CV of adult survey errors on log scale
		g0			   ="numeric",		## log(growth) in year 1
		g.sigma		 ="numeric",		## sd(Z_devs) 
		stock0 		 ="numeric"),		## biomass in starting year t=1
	prototype=prototype(
		range		   =c(minyear=as.numeric(NA),		maxyear=as.numeric(NA)),	
		r.mu		   =c(min=1,		 max=10000000,	phase=1),	
		r.cv		   =c(min=-6,		 max=10,			  phase=1),	 
		r.index.q	 =c(min=0.001, max=1,			    phase=1),	 
		r.index.cv =c(min=0.02,	 max=2,			    phase=2),	 
		adult.q		 =c(min=0.01,	 max=1,			    phase=-1), 
		adult.cv	 =c(min=0.02,	 max=2,			    phase=-1), 
		g0			   =c(min=-5,		 max=0.01,		  phase=2),	 
		g.sigma		 =c(min=-6,		 max=0,			    phase=1),	 
		stock0		 =c(min=1,		 max=50000000,	phase=1)), 
 	validity=validFLBREM.control
)

setValidity("FLBREM.control", validFLBREM.control)
remove(validFLBREM.control)	# We do not need this function any more


## FLBREM ######################################################################
validFLBREM <- function(object){
	# All FLQuant objects must have same dimensions
	Dim <- dim(object@z)
	if (!all(dim(object@z) == Dim))
		return("arrays must have same dimensions")

	# Everything is fine
	return(TRUE)
}

setClass("FLBREM",
	representation(
		control   ="FLBREM.control",
		call      ="character",
		desc      ="character",
    g      	  ="FLQuant",   ## total annual mortality (1,m-1)
    stock.n   ="FLQuant",      
		r.mu	  	="numeric",		## average recruitment
		r.cv		  ="numeric",		## cv recruitement logNormal scale
		r.index.q	="numeric",		## Catchability recruits (survey specific)
		r.index.cv="numeric",		## CV of recruit survey errors on log scale
		adult.q		="numeric",		## Catchability adults (survey specific)
		adult.cv	="numeric",		## CV of adult survey errors on log scale
		g0			  ="numeric",		## log(growth) in year 1
		g.sigma		="numeric",		## sd(Z_devs) 
		stock0		="numeric",		## biomass in starting year t=1
    obj       ="numeric"),
	prototype=prototype(
   		control   =new("FLBREM.control"),
   		call      ="new(\"FLBREM\")",
   		desc      =character(0),
      g      	  =as.FLQuant(NA),    ## total annual mortality (1,m-1)
      stock.n   =as.FLQuant(NA),      
		  r.mu		  =as.numeric(NA),		## average recruitment
		  r.cv		  =as.numeric(NA),		## cv recruitement logNormal scale
		  r.index.q	=as.numeric(NA),		## Catchability recruits (survey specific)
		  r.index.cv=as.numeric(NA),		## CV of recruit survey errors on log scale
		  adult.q		=as.numeric(NA),		## Catchability adults (survey specific)
		  adult.cv	=as.numeric(NA),		## CV of adult survey errors on log scale
		  g0			  =as.numeric(NA),		## log(growth) in year 1
		  g.sigma		=as.numeric(NA),		## sd(Z_devs) 
		  stock0		=as.numeric(NA),		## biomass in starting year t=1
      obj.val   =as.numeric(NA)),
	validity=validFLBREM
)

setValidity("FLBREM", validFLBREM)
remove(validFLBREM)	# We do not need this function any more

is.FLBREM<-function(object) {
   return(TRUE)
}

### End classes ################################################################


### Methods #############################################################
FLBREM <- function(recruit, adult, control="missing", desc){
	Call <- match.call()

#	if (!inherits(recruit, "FLIndices"))
#		stop("FLIndices must be an 'FLIndices' object!")
#	if (!validObject(FLIndices))
#		stop("FLIndices is not valid!")

	if (!inherits(recruit, "FLIndexSurvey"))
		stop("recruit must be an 'FLIndexSurvey' object!")
	if (!validObject(FLIndices))
		stop("recruit is not valid!")
   
	if (!inherits(adult, "FLIndexSurvey"))
		stop("adult must be an 'FLIndexSurvey' object!")
	if (!validObject(FLIndices))
		stop("adult is not valid!")

  if (missing(control))
		control<-FLBREM.control()
	else if (!validObject(control))
		stop("control is not valid!")
	
	if (!missing(desc)) res@desc <- as.character(desc)

	if (!inherits(control, "FLBREM.control"))
		stop("control must be an 'FLBREM.control' object!")
	else if (missing(control))
		control<-FLBREM.control(FLStock)
		
  ## check range
  if (adult@range["minyear"] >= recruit@range["minyear"]) recruit@range["minyear"] = adult@range["minyear"]+1
  
  data.minyear<-min(adult@range["minyear"], recruit@range["minyear"])
  data.maxyear<-max(adult@range["maxyear"], recruit@range["maxyear"])
 
  range<-c(minyear=ifelse(any(names(control@range)=="minyear"),control@range["minyear"],data.minyear), 
           maxyear=ifelse(any(names(control@range)=="maxyear"),control@range["maxyear"],data.maxyear))
   
  if (is.na(range["minyear"])) range["minyear"] <- data.minyear
  if (is.na(range["maxyear"])) range["maxyear"] <- data.maxyear
  
  range["minyear"] = max(data.minyear, range["minyear"])
  range["maxyear"] = min(data.maxyear, range["maxyear"])
	
	control@range <- range
  
  ## run it		
  recruit<-FLIndices(recruit)
  adult  <-FLIndices(adult)

  owarn <- options()$warn.FPU
  options(warn.FPU=FALSE)

  dyn.load(paste(R.home(),"/library/FLBREM/libs/BREM.dll",sep=""))

  if (FALSE){
      RecIndex  <-c(1:dims(recruit[[1]])$years+1,rep(1,dims(recruit[[1]])$years),as.double(recruit[[1]]@index))
      AdultIndex<-c(1:dims(adult[[1]])$years,    rep(1,dims(adult[[1]])$years),  as.double(adult[[1]]@index))

## return(list(RecIndex,AdultIndex))

     res<-.C("bmod2sd",
              m=as.integer(dims(adult[[1]])$years),
              nAdult=as.integer(1), LenAdult=as.integer(dims(adult[[1]])$years),  Adult=as.double(AdultIndex), 
              nRec  =as.integer(1), LenRec  =as.integer(dims(recruit[[1]])$years),Rec  =as.double(RecIndex), 
              g.hat=rep(as.double(0),dims(adult[[1]])$years),
              r.hat=rep(as.double(0),dims(adult[[1]])$years),
              b.hat=rep(as.double(0),dims(adult[[1]])$years),
              rmu=as.double(control@r.mu),
            	rcv=as.double(control@r.cv),		 
              rq=as.double(control@r.index.q),	 
            	ricv=as.double(control@r.index.cv), 
            	aq=as.double(control@adult.q),		 
            	acv=as.double(control@adult.cv),	 
            	g0=as.double(control@g0),			   
            	gsigma=as.double(control@g.sigma),		 
            	stock0=as.double(control@stock0),
              " -nox -nohess")
  
      return(res)
  } else {
      res      <- .Call("_FLBREM", recruit, adult, control)      
      
      res@g                     <-FLQuant(res@g)
      res@g[1,dims(res@g)$year] <-NA
      res@stock.n               <-FLQuant(res@stock.n)      
      res@stock.n[1,1]          <-NA

      dimnames(res@g)[[1]]       <-c("all")
      dimnames(res@stock.n)[[1]] <-c("rec","adult")
    }  	

  dyn.unload(paste(R.home(),"/library/FLBREM/libs/BREM.dll",sep=""))
  options(warn.FPU=owarn)
  
  res@call    <- as.character(Call)
 	res@control <- control

  #if (control@r.mu["phase"]       == -1)  res@r.mu        <-control@r.mu["min"]
  #if (control@r.cv["phase"]       == -1)  res@r.cv        <-control@r.cv["min"]		  
	#if (control@r.index.q["phase"]  == -1)  res@r.index.q   <-control@r.index.q["min"]	
	#if (control@r.index.cv["phase"] == -1)  res@r.index.cv  <-control@r.index.cv["min"]
	#if (control@adult.q["phase"]    == -1)  res@adult.q     <-control@adult.q["min"]		
	#if (control@adult.cv["phase"]   == -1)  res@adult.cv    <-control@adult.cv["min"]	
	#if (control@g0["phase"]         == -1)  res@g0          <-control@g0["min"]			  
	#if (control@g.sigma["phase"]    == -1)  res@g.sigma     <-control@g.sigma["min"]		
	#if (control@stock0["phase"]     == -1)  res@stock0      <-control@stock0["min"]		

 	if (!missing(desc)) res@desc <- as.character(desc)
  
  return(res)
}

FLBREM.control <- function(FLBREM=NULL,
  						  range       =c(minyear=as.double(NA), maxyear=as.double(NA)),
								r.mu		    =c(min=1,		  max=10000000,	phase=1),	
      					r.cv		    =c(min=-6,		max=10,			phase=1),	
			  				r.index.q	  =c(min=0.001,	max=1,			phase=1),	
				  			r.index.cv	=c(min=0.02,	max=2,			phase=2),	
					  		adult.q		  =c(min=0.01,	max=1,			phase=-1),	
						   	adult.cv	  =c(min=0.02,	max=2,			phase=-1),	
							  g0			    =c(min=-5,		max=0.01,		phase=2),	
							  g.sigma		  =c(min=-6,		max=0,			phase=1),	
							  stock0		  =c(min=1,		  max=50000000,	phase=1)){

  ## fixes parameter
  fix.param<-function(x){
    if (length(x)==1 && !any(names(x) == c("min","max","phase")))
      return(c(min=as.double(x-abs(x)*1e-5),max=as.double(x+abs(x)*1e-5),phase=as.double(-1)  ))
    else
      return(x)
    }
  
	if (is.null(FLBREM)){
		res<-new("FLBREM.control")} 
	else if (inherits(FLBREM, "FLBREM")) {
    res<-FLBREM@control}
	else
		stop("FLBREM must be an 'FLBREM' object!")

	res@range		    <-range		 
	res@r.mu		    <-fix.param(r.mu)		 
	res@r.cv		    <-fix.param(r.cv)		 
	res@r.index.q	  <-fix.param(r.index.q)	 
	res@r.index.cv	<-fix.param(r.index.cv)	 
	res@adult.q		  <-fix.param(adult.q)		 
	res@adult.cv	  <-fix.param(adult.cv)	 
	res@g0			    <-fix.param(g0)			 
	res@g.sigma		  <-fix.param(g.sigma)		 
	res@stock0		  <-fix.param(stock0)
	
	# Verify that this object is valid
	test <- validObject(res)
	 	if (!test)
			stop("Invalid object:", test)

	return(res)
	}

### End methods ################################################################  
