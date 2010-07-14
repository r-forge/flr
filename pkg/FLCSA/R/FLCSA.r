### FLR core version 0.7-0 ###
### FLCSA
### This file contains code just for the core class FLCSA

### 17-04-2004 implemented as a S4 Class L T Kell & Dave Bromley
### based upon original S+ code by Benoit Mesnil 

### class ##############################################################

validFLCSA.control <- function(object){

	# Everything is fine
	return(TRUE)}

	setClass("FLCSA.control",
		representation(
			tau          ="numeric",
			lambda       ="numeric",
			qratio       ="numeric",
			qini         ="numeric",
			ftol         ="numeric",
			ptol         ="numeric",
			gtol         ="numeric",
			diag         ="numeric",
			factor       ="numeric",
			nprint       ="numeric"),
		prototype=prototype(
			tau          =as.double(0.5),
			lambda       =as.double(1.0),
			qratio       =as.double(0.995),
			qini         =as.double(1.0),
			ftol         =as.numeric(0.00001),
			ptol         =as.numeric(0.00001),
			gtol         =as.numeric(0.00001),
			diag         =numeric(),
			factor       =as.integer(100),
			nprint       =as.integer(0)),
		validity=validFLCSA.control)

setValidity("FLCSA.control", validFLCSA.control)
remove(validFLCSA.control)	# We do not need this function any more


## FLCSA ######################
validFLCSA <- function(object){
	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@f) == Dim))
		return("n and f arrays must have same dimensions")

  # Everything is fine
	return(TRUE)}

setClass("FLCSA",
	representation(
		stock.n  ="FLQuant",
		f        ="FLQuant",
		b        ="FLQuant",
		qhat     ="numeric",
		qres     ="FLQuant",
		obj      ="numeric",
		control  ="FLCSA.control",
		call     ="character",
		desc     ="character"),
	prototype=prototype(
		stock.n  =new("FLQuant"),
		f        =new("FLQuant"),
		b        =new("FLQuant"),
		qhat     =numeric(0),
		qres     =new("FLQuant"),
		obj      =numeric(0),
		control  =new("FLCSA.control"),
		call     ="new(\"FLCSA\")",
		desc     =character(0)),
	validity=validFLCSA)

setValidity("FLCSA", validFLCSA)
remove(validFLCSA)	# We do not need this function any more
### End class ###########################################################


### Methods #############################################################
FLCSA <- function(FLStock, FLIndices, control=FLCSA.control(), desc){
	Call <- match.call()
	
	if (!inherits(FLStock, "FLStock"))
		stop("FLStock must be an 'FLStock' object!")

	if (!inherits(FLIndices, "FLIndices")){
		if (!inherits(FLIndices, "FLIndex"))
		    stop("FLIndices must be an 'FLIndices' or a 'FLIndex' object!")
		else
		    i.<-FLIndices}
	else
		i.<-FLIndices[[1]]

	if (!inherits(control, "FLCSA.control"))
		stop("control must be an 'FLCSA.control' object!")


   data<-prepare.data(s.,i.,control@qratio)

   #  STEP 3: Set constants
   #  =====================
   nyr   <- length(data$Year)  # nbr of Years
   npar  <- nyr                # nbr of parameters
   nrt   <- 2*nyr-1            # nbr of residual terms

   #  STEP 4: Choose options for this CSA run (re-enter here for replays)
   #  ========================================
   # (i) Tau ; 0.5 for Catch discounted mid-year, or O for Catch at year start
   tau <- control@tau

   # (ii) Weight of error on Recruits indices, relative to error on FullRec indices
   lambda <- control@lambda
   qini   <- control@qini
   vini   <- CSiniGM.f(data,qini,tau)
   #CSmin <- optim(par=vini,fn=CSresGM.f,lower=0.000002, data=data,tau=tau,lambda=lambda, method="L-BFGS-B")
   maxfev<-100*(npar+1)
   mqdt.control<-list(ftol=control@ftol, ptol=control@ptol,gtol=control@gtol,diag=control@diag,factor=control@factor,maxfev=maxfev,nprint=control@nprint)
   #mqdt.control <- list(ftol=0.00001,ptol=0.00001,gtol=0,diag=numeric(),factor=100,maxfev=100*(npar+1),nprint=0)
   CSmin  <- nls.lm(vini,fn=CSresGM.f,control=mqdt.control,data=data,tau=tau,lambda=lambda)
#return(list(CSmin=CSmin,data=data,tau=tau,lambda=lambda,desc=FLStock@desc))
   CSest  <- CSoutGM.f(CSmin,data,tau,lambda,FLStock@desc)

   results         <-new("FLCSA")	
   results@call    <- as.character(Call)
	 results@desc    <- as.character(paste("FLCSA:",FLStock@desc))

   results@stock.n <- as.FLQuant(array(t(cbind(CSest$Stock[,"RecN"], CSest$Stock[,"FullN"])), dim=c(2,length(CSest$Stock[,"FullN"])), dimnames=list(age=c("r","full"),year=CSest$Stock[,"Year"])))
   results@f       <- results@stock.n*FLStock@stock.wt
   results@b       <- as.FLQuant(array(t(cbind(CSest$Stock[,"RecN"], CSest$Stock[,"FullN"])), dim=c(2,length(CSest$Stock[,"FullN"])), dimnames=list(age=c("r","full"),year=CSest$Stock[,"Year"])))
   results@qhat    <- CSest$qhat
   results@control <- control


   w              <- sqrt(lambda)  # Watch new use of w
 	 r.residuals    <- w*log(ifelse(data$Urec>0,(results@qhat*data$Srat*CSest$Stock[,"RecN"]/data$Urec),1))          # Recruits
	 full.residuals <- log(ifelse(data$Ufull>0,(results@qhat*CSest$Stock[,"FullN"]/data$Ufull),1))   # Full
   results@qres   <- as.FLQuant(array(t(cbind(r.residuals,full.residuals)),dim=c(2,length(full.residuals)),dimnames=list(age=c("r","full"),year=CSest$Stock[,"Year"])))

   return(results)
   }

FLCSA.control <- function(FLCSA=NULL, tau=0.5, lambda=1.0, qratio=1.0, qini=1.0,ftol=0.00001, ptol=0.00001,gtol=0,diag=numeric(),factor=100,nprint=0){
	if (is.null(FLCSA)){
		res <- new("FLCSA.control", tau=tau, lambda=lambda, qratio=qratio, qini=qini,ftol=0.00001, ptol=0.00001,gtol=0,diag=numeric(),factor=100,nprint=0)
	} else {	# We reuse an FLCSA.control object embedded in an FLCSA object
		if (!inherits(FLCSA, "FLCSA"))
			stop("FLCSA must be an 'FLCSA' object!")
		res <- FLCSA@control
		# ... and possibly redefine some of its parameters
		if (!missing(tau))
			res@tau <- tau
		if (!missing(lambda))
			res@lambda <- lambda
		if (!missing(qratio))
			res@qratio <- qratio
		if (!missing(qini))
			res@qini <- qini
		# Verify that this object is valid
		test <- validObject(res)
		if (!test)
			stop("Invalid object:", test)
	}
	return(res)
}

## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLCSA.control"),
	function(object){
      n.<-slotNames(object)
	   for (i in 1:length(n.))
         cat(n.[i],"\t\t",slot(object,n.[i]),"\n")
	}
)

# Creator of FLCSA objects, FLCSA() is defined in Calls.R

# Test if an object is of FLCSA class
is.FLCSA <- function(x)
	return(inherits(x, "FLCSA"))

# Test if an object is of FLCSA.control class
is.FLCSA <- function(x)
	return(inherits(x, "FLCSA.control"))

# Coercion... I don't see to what, or from what this complex object
# could currently be coerced... may be when future versions appear...

## show (the default method)
# Rem: not needed...
#setMethod("show", signature(object="FLCSA"),
#	function(object){
#		cat("An object of class \"FLCCSA\":\n\n")
#		# Add more info here?
#		print(unclass(object))
#	}
#)

## summary
setMethod("summary", signature(object="FLCSA"),
	function(object, ...){
		cat("An object of class \"FLCSA\" with:\n\n")
		cat("Dimensions:\n")
		print(dimnames(object@stock.n))
		cat("Control parameters:\n")
		summary(object@control)
		# Should we add some summary stats for the different slots here?
	})

setMethod("plot", signature(x="FLCSA", y="missing"),
	function(x, y, type=c("1", "2"), cols=c(1,2,8,4), ...){


   CSplot1.f <- function(x,cols){
      # Plots Biomass & Recruits N + Barplot of residuals - March 04 
      #
      #parsave <- par()          # This is normally recommended but
      #	on.exit(par(parsave))  #  gives a silly warning, known as bug in par() help
      par(mfcol=c(2,2))
      par(oma=c(1,1,1,1))
      par(mar=c(4,4,2,2)+0.1)
      
      # Recruits
      plot(dimnames(x@stock.n)[[2]],x@stock.n["r",,drop=TRUE],type="b",xlab="",ylab="Recruits (M)",las=1)
      
      # Biomass
      plot(dimnames(x@b)[[2]],apply(x@b,2,sum),type="b",xlab="Year",ylab="Biomass (,OOO t)",las=1)   #,tck=0.02)

      # Residuals
      nyr <- length(dimnames(x@stock.n)[[2]])
      nrt <- length(dimnames(x@qres)[[2]])

      ## NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
      
      #   x-axis labels every 5 years
      xleg <- as.character(dimnames(x@stock.n)$year[1:(nyr-1)])
      xleg[(1:(nyr-1)%%5)!=0] <- " "
      barplot(x@qres[1,1:(nyr-1),drop=TRUE],xlab="",ylab="Raw log-residuals Recruits",las=1)
      xleg <- dimnames(x@stock.n)$year
      xleg[(1:nyr%%5)!=0] <- " "
      barplot(x@qres[2,1:nyr,drop=TRUE], xlab="Year",ylab="Raw log-residuals FullRecr.",las=1)
      mtext(x@desc,side=3,outer=T)
      }


   CSplot2.f <- function(x,cols){
      # Plots Biomass & Recruits N + Line plot of residuals
      #

      #parsave <- par()          # This is normally recommended but
      #	on.exit(par(parsave))  #  gives a silly warning, known as bug in par() help
      par(mfcol=c(2,2))
      par(oma=c(1,1,1,1))
      par(mar=c(4,4,2,2)+0.1)

      # Recruits
      plot(dimnames(x@stock.n)[[2]],x@stock.n["r",,drop=TRUE],type="b",xlab="",ylab="Recruits (M)",las=1)

      # Biomass
      plot(dimnames(x@stock.n)[[2]],apply(x@b,2,sum),type="b",xlab="Year",ylab="Biomass (,OOO t)",las=1)   #,tck=0.02)

      # Residuals
      nyr <- length(dimnames(x@stock.n)[[2]])
      nrt <- length(x@qres["r",,drop=TRUE])

      # NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
      plot(dimnames(x@stock.n)[[2]][1:(nyr-1)],x@qres["r",1:(nyr-1),drop=TRUE],type="p",pch=18,
      xlab="",ylab="Raw log-residuals Recruits",las=1)
      abline(h=0.0,lty=4)
      plot(dimnames(x@stock.n)[[2]],x@qres["full",1:nyr],type="p",pch=18,
      xlab="Year",ylab="Raw log-residuals FullRecr.",las=1)
      abline(h=0.0,lty=4)
      mtext(x$desc,side=3,outer=T)
      }
      
		
	if (length(cols) < 4)
		cols <- rep(cols, 4)

	opar <- par(no.readonly = TRUE)
	on.exit(par(opar))
	switch(type[1],
		"1"      =CSplot1.f(x, cols),
		"2"  	   =CSplot2.f(x, cols),
		cat("type must be '1', '2'!\n"))
	      invisible(NULL)
	}
)

## update (use a stock assessment to update n and f)
if (!isGeneric("update")) {
    setGeneric("update", function(object, ...){
	    value <- standardGeneric("update")
	    value
    })
}

setMethod("update", signature(object="FLCSA"),
      function(object, FLStock, ...){
		# Currently works only with FLCSA and FLAdapt FLStocks
		if (missing(object))
			stop("object must be an FLCSA or FLAdapt FLStock")
		if (!inherits(object, "FLCSA") && !inherits(object, "FLAdapt"))
			stop("object must be an FLCSA or FLAdapt FLStock")
		# Check that dims in FLCSA or FLAdapt is a subset of dims in the FLStock
		isSubset <- function(dnX, dnY) {
			# We consider here that we have five dimensions, like in usual FLQuant FLStocks
			if (!all(dnX$age    %in% dnY$age))    return(FALSE)
			if (!all(dnX$year   %in% dnY$year))   return(FALSE)
			if (!all(dnX$unit   %in% dnY$unit))   return(FALSE)
			if (!all(dnX$season %in% dnY$season)) return(FALSE)
			if (!all(dnX$area   %in% dnY$area))   return(FALSE)
			return(TRUE)
		}
		dnOn <- dimnames(FLStock@stock.n)
		dnAn <- dimnames(object@stock.n)
		if (!isSubset(dnAn, dnOn))
			stop("n is object is not a subset of n is the stock FLStock!")
		if (!all(dim(object@f) == dim(object@stock.n)))
			stop("arrays n and f must have same dimensions in object!")
		# Create the updated stock FLStock
		res <- FLStock
		if (inherits(object, "FLCSA")){
			res@desc <- paste(FLStock@desc, "- updated according to an FLCSA with desc:", object@desc)
		} else {	# This should be an FLAdapt
			res@desc <- paste(FLStock@desc, "- updated according to an FLAdapt with desc:", object@desc)
		}
		# We now subset the FLStock according to dimensions in the object
		doSubset <- function(Quant, Dimnames, do.subset.age=TRUE) {
			# We subset an FLQuant FLStock according to Dimnames (supposed to ba a strict subset tested with isSubset())
			if (do.subset.age) {
				res <- as.FLQuant(Quant[Dimnames$age, Dimnames$year, Dimnames$unit, Dimnames$season, Dimnames$area, drop=FALSE])
			} else {
				res <- as.FLQuant(Quant[, Dimnames$year, Dimnames$unit, Dimnames$season, Dimnames$area, drop=FALSE])
			}
			res
		}
		res@mat     <- doSubset(FLStock@mat,      dnAn)
		res@catch.n   <- doSubset(FLStock@catch.n,    dnAn)
		res@discard <- doSubset(FLStock@discard,  dnAn)
		res@bycatch <- doSubset(FLStock@bycatch,  dnAn)
		res@m       <- doSubset(FLStock@m,        dnAn)
		res@stock.wt     <- doSubset(FLStock@stock.wt,      dnAn)
		res@catch.wt     <- doSubset(FLStock@catch.wt,      dnAn)
		res@f.spwn   <- doSubset(FLStock@f.spwn,    dnAn)
		res@m.spwn   <- doSubset(FLStock@m.spwn,    dnAn)
		res@yield   <- doSubset(FLStock@yield,    dnAn, do.subset.age=FALSE)
		# This is the updated fields

		res@stock.n       <- object@stock.n
		res@f       <- object@f
		
      # Range is set to the value in res@stock.n
		Par         <- params(res@stock.n)
		res@range <- unlist(list(minage=Par$minage, maxage=Par$maxage, plusgroup=Par$maxage, minyear=Par$minyear, maxyear=Par$maxyear))
		return(res)
	}
)

## update (use a stock assessment to update n and f)
if (!isGeneric("update")) {
    setGeneric("update", function(object, ...){
	    value <- standardGeneric("update")
	    value
    })
}

setMethod("update", signature(object="FLCSA"),
      function(object, FLStock, ...){
		
      if (missing(object))
			stop("object must be an FLCSA")
		if (!inherits(object, "FLCSA"))
			stop("object must be an FLCSA")
		# Check that dims in FLStock match dims in the FLStock
		
      isSubset <- function(dnX, dnY) {
			# We consider here that we have five dimensions, like in usual FLQuant FLStocks
			if (!all(dnX$age    %in% dnY$age))    return(FALSE)
			if (!all(dnX$year   %in% dnY$year))   return(FALSE)
			if (!all(dnX$unit   %in% dnY$unit))   return(FALSE)
			if (!all(dnX$season %in% dnY$season)) return(FALSE)
			if (!all(dnX$area   %in% dnY$area))   return(FALSE)
			return(TRUE)
		   }

      if (dims(FLStock@m)[1] != 2)
         {
         .c              <-FLStock@catch.wt[2:nrow(FLStock@catch.wt),]*FLStock@catch.n[2:nrow(FLStock@catch.n),]
         FLStock@catch.wt     <-as.FLQuant(array(t(cbind(FLStock@catch.wt[1,],apply(.c,2,sum)/apply(FLStock@catch.n[2:nrow(FLStock@catch.n),],2,sum))),     dim=c(2,ncol(FLStock@catch.wt)),    dimnames=list(c("r","full"),dimnames(FLStock@catch.wt)$year)))
         .s              <-FLStock@stock.wt[2:nrow(FLStock@stock.wt),]*FLStock@catch.n[2:nrow(FLStock@catch.n),]
         FLStock@stock.wt     <-as.FLQuant(array(t(cbind(FLStock@stock.wt[1,],apply(.c,2,sum)/apply(FLStock@catch.n[2:nrow(FLStock@catch.n),],2,sum))),     dim=c(2,ncol(FLStock@stock.wt)),    dimnames=list(c("r","full"),dimnames(FLStock@stock.wt)$year)))
         FLStock@catch.n   <-as.FLQuant(array(t(cbind(FLStock@catch.n[1,],apply(FLStock@catch.n[2:nrow(FLStock@catch.n),],2,sum))),dim=c(2,ncol(FLStock@catch.n)),dimnames=list(c("r","full"),dimnames(FLStock@catch.n)$year)))
         FLStock@mat     <-as.FLQuant(array(t(cbind(FLStock@mat[1,],      apply(FLStock@mat[2:nrow(FLStock@m.spwn),],2,mean))),       dim=c(2,ncol(FLStock@mat)),      dimnames=list(c("r","full"),dimnames(FLStock@mat)$year)))
         FLStock@m       <-as.FLQuant(array(t(cbind(FLStock@m[1,],    apply(FLStock@m[2:nrow(FLStock@m),],2,mean))),     dim=c(2,ncol(FLStock@m)),    dimnames=list(c("r","full"),dimnames(FLStock@m)$year)))
         FLStock@f.spwn   <-as.FLQuant(array(t(cbind(FLStock@f.spwn[1,],    apply(FLStock@f.spwn[2:nrow(FLStock@f.spwn),],2,mean))),     dim=c(2,ncol(FLStock@f.spwn)),    dimnames=list(c("r","full"),dimnames(FLStock@f.spwn)$year)))
         FLStock@m.spwn   <-as.FLQuant(array(t(cbind(FLStock@m.spwn[1,],    apply(FLStock@m.spwn[2:nrow(FLStock@m.spwn),],2,mean))),     dim=c(2,ncol(FLStock@m.spwn)),    dimnames=list(c("r","full"),dimnames(FLStock@m.spwn)$year)))
         }

		dnOn <- dimnames(FLStock@stock.n)
		dnAn <- dimnames(object@stock.n)
		if (!isSubset(dnAn, dnOn))
			stop("n is object is not a subset of n is the stock FLStock!")
		if (!all(dim(object@f) == dim(object@stock.n)))
			stop("arrays n and f must have same dimensions in object!")
		# Create the updated stock FLStock
		res <- FLStock
		if (inherits(object, "FLCSA"))
			res@desc <- paste(FLStock@desc, "- updated according to an FLCSA with desc:", object@desc)
		   
		# We now subset the FLStock according to dimensions in the object
		doSubset <- function(Quant, Dimnames, do.subset.age=TRUE) 
         {
			# We subset an FLQuant FLStock according to Dimnames (supposed to ba a strict subset tested with isSubset())
			if (do.subset.age) 
				   res <- as.FLQuant(Quant[Dimnames$age, Dimnames$year, Dimnames$unit, Dimnames$season, Dimnames$area, drop=FALSE])
			else 
				res <- as.FLQuant(Quant[, Dimnames$year, Dimnames$unit, Dimnames$season, Dimnames$area, drop=FALSE])
			
			res
		   }

		res@mat     <- doSubset(FLStock@mat,      dnAn)
		res@catch.n   <- doSubset(FLStock@catch.n,    dnAn)
		res@discard <- doSubset(FLStock@discard,  dnAn)
		res@bycatch <- doSubset(FLStock@bycatch,  dnAn)
		res@m       <- doSubset(FLStock@m,        dnAn)
		res@stock.wt     <- doSubset(FLStock@stock.wt,      dnAn)
		res@catch.wt     <- doSubset(FLStock@catch.wt,      dnAn)
		res@f.spwn   <- doSubset(FLStock@f.spwn,    dnAn)
		res@m.spwn   <- doSubset(FLStock@m.spwn,    dnAn)
		res@yield   <- doSubset(FLStock@yield,    dnAn, do.subset.age=FALSE)
		# This is the updated fields

		res@stock.n       <- object@stock.n
		res@f       <- object@f
		
      # Range is set to the value in res@stock.n
		Par         <- params(res@stock.n)
		res@range <- unlist(list(minage=Par$minage, maxage=Par$maxage, plusgroup=Par$maxage, minyear=Par$minyear, maxyear=Par$maxyear))
		return(res)
	}
)

# project
if (!isGeneric("project")) {
    setGeneric("project", function(object, ...){
	    value <- standardGeneric("project")
	    value
    })
}

#setMethod("project", signature(object="FLCSA"),
#      function(object, control=FLCSA.proj.control(), ...){
		
#      if (missing(object) || !inherits(object, "FLCSA"))
#			stop("object must be an FLCSA")

   #FLStock@stock.n
   #FLStock@f
   #FLStock@m
   #FLStock@catch.n
   #FLStock@catch.wt
   #FLStock@stock.wt
   #FLStock@mat
   		
   #for(i in minprojyear:(maxprojyear-1))  {
   #   FLStock@stock.n["full",i+1] <- FLStock@stock.n["r",i]*exp(-FLStock@stock.n["r",i]) 
   #                          - FLStock@catch.n["r",i] 
   #                          + FLStock@stock.n["full",i])*exp(-FLStock@m["full",i]) 
   #                          - FLStock@catch.n["full",i]
   #   }
   
#	return(FLStock)
#	}
#)

setMethod("+", signature(e1="FLStock",e2="FLCSA"),
    function(e1, e2) {
	    #need to check plusgroup etc.
        e1@stock.n<-e2@stock.n
	    e1@f       <-e2@f

        return(e1)   
	})


setMethod("+", signature(e1="FLCSA",e2="FLStock"),
    function(e1, e2) {
        #need to check plusgroup etc.
        e2@stock.n<-e1@stock.n
	    e2@f      <-e1@f

        return(e2)   
	})

### End methods ###########################################################

## Internal Functions ##########################################################
# ------
CSoutGM.f <- function(liste,data,tau,lambda,title){
	#  Version for q estimated as GM, not searched
	# Computes CSA stock estimates and harvest rates by stage
	# based on output of minimisation (in liste) and stuff in data;
	# Assembles output in a list
	# Calls CSfwd.f  (thus needs to pass tau)
	#
	# Amended for missing (< 0) indices (12/12/03)
	# Wrong calculation of qhat modified 03/09/03 & 12/03/04 => now cumbersome
	## This is Version for R with nls.lm (just change names in liste$..); Feb 05
	#

	nyr    <- length(data$Year)
	RecN   <- vector(length = nyr)
	FullN  <- vector(length = nyr)
	Btot   <- vector(length = nyr)
	HRrec  <- vector(length = nyr)
	HRfull <- vector(length = nyr)
	
	#
	RecN        <- liste$par              # Recruits N (but last year's wrong)
	names(RecN) <- NULL    # needed in R to remove unwanted 'row' labels in out
	FullN       <- CSfwd.f(liste$par,data,tau,nyr)   # Full N
	tmp         <- data$Urec[1:(nyr-1)]/(data$Srat[1:(nyr-1)]*RecN[1:(nyr-1)])
	w           <- sum(ifelse(tmp>0,lambda,0))+ sum(ifelse(data$Ufull>0,1,0))
	qhat        <- sum(lambda*log(ifelse(tmp>0,tmp,NA)),na.rm=T)
	qhat        <- qhat+ sum(log(ifelse(data$Ufull>0,data$Ufull/FullN,NA)),na.rm=T)
	qhat        <- exp(qhat/w)

  #  Use observed Urec (or mean if missing) for last recruitment
	ubar        <- mean(data$Urec[data$Urec>0])
	RecN[nyr]   <- ifelse(data$Urec[nyr]>0,data$Urec[nyr],ubar)/(qhat*data$Srat[nyr])
	Btot        <- (RecN*data$Wrec)+(FullN*data$Wfull)
	HRrec       <- data$CatRec/RecN
	HRfull      <- data$CatFull/FullN
	Year        <- data$Year

	Stock       <- data.frame(Year=Year,RecN=RecN[drop=T],FullN=FullN[drop=T],Btot=Btot[drop=T],HRrec=HRrec[drop=T],HRfull=HRfull[drop=T])
	out         <- list(descrip=title,tau=tau,lambda=lambda,M=data$M,Stock=Stock,qhat=qhat)

  return(out)}

CSfwd.f <- function(Vpar,data,tau,nyr){
	# Project CSA equation forward to estimate fully recruited Ns
	# given RecruitsN in Vpar[1 : nyr-1] and first year's FullN in Vpar[nyr]
	# and total catch in column $CatchN of df data
	#
	Nfull <- vector(length = nyr)
	Nfull[1] <- Vpar[nyr]

	for(i in 1:(nyr-1)) {
		natm <- data$M[i]
		x          <- (Vpar[i] + Nfull[i])*exp(-natm)
		Nfull[i+1] <- x - data$CatchN[,i]*exp(-natm*(1-tau))}

	Nfull}


#      --------------------------------
##  Functions used:
#      -- ------------------------------
#
CSiniGM.f <- function(data,qini,tau){
	#  Version for q estimated as GM, not searched
	# Initialise CSA parameter vector given observed indices
	# replace missing (< 0) indices by mean of non-missing
	# rescale qini until Pop > Catch
	#
	nyr <- length(data$Year)
	v <- vector(length = nyr)
	tmp <- rep(-1,nyr)
	ubar1 <- mean(data$Urec[data$Urec>0])
	ubar2 <- mean(data$Ufull[data$Ufull>0])
	qini <- qini*10

	while(any(tmp<0)) {
      	qini     <- qini/10
      	v[1:nyr] <- (ifelse(data$Urec>0,data$Urec,ubar1))/(qini*data$Srat)  # Recruits
      	v[nyr]   <- (ifelse(data$Ufull[1]>0,data$Ufull[1],ubar2))/qini   # 1st year full
      	tmp      <- CSfwd.f(v,data,tau,nyr)}

	lab <- paste("R",1:nyr,sep="")
	lab[nyr] <- "N1"
	names(v) <- lab
	# save final qini, and return v
	qini <<- qini
	v
}

# ------
CSresGM.f <- function(Vpar,data,tau,lambda){
	#  Version for q estimated as GM, not searched
	# Computes & returns CSA (log-)residuals
	# Vpar is vector of 'current' parameter values, with RecruitsN in
	#  positions [1:(nyr-1)] and 1st year's FullN in position [nyr]
	# Version for nlregb: residuals not squared  (but should for ms())
	# Calls CSfwd.f => needs to pass tau
	#
	# Amended for missing (< 0) indices; 12/12/03
	# BUT nlregb() does NOT accept NA residuals => artificially forced to zero (log(1))
	# however, same CSA results obtained when corresponding residuals are excluded
	# Wrong calculation of qhat modified 03/09/03 & 12/03/04 => now cumbersome
	#
	nyr <- length(data$Year)
	nres <- 2*nyr-1
	vres <- vector(length = nres)

	Nfull <- CSfwd.f(Vpar,data,tau,nyr)
	tmp <- data$Urec[1:(nyr-1)]/(data$Srat[1:(nyr-1)]*Vpar[1:(nyr-1)])
	w <- sum(ifelse(tmp>0,lambda,0))+ sum(ifelse(data$Ufull>0,1,0))
	qhat <- sum(lambda*log(ifelse(tmp>0,tmp,NA)),na.rm=T)
	qhat <- qhat+ sum(log(ifelse(data$Ufull>0,data$Ufull/Nfull,NA)),na.rm=T)
	qhat <- exp(qhat/w)
	w <- sqrt(lambda)  # Watch new use of w
	vres <- w*log(ifelse(data$Urec>0,(qhat*data$Srat*Vpar/data$Urec),1))  # Recruits
	vres[nyr:nres] <- log(ifelse(data$Ufull>0,(qhat*Nfull/data$Ufull),1))   # Full
	vres
}

  prepare.data<-function(s.,i.,qratio){

  years   = max(s.@range["minyear"],i.@range["minyear"]):
            min(s.@range["maxyear"],i.@range["maxyear"])

  s.<-trim(s.,year=years)
  i.<-trim(i.,year=years)

  if (length(dimnames(s.@catch.n)$ages)==2){
	   Ufull  =i.@index[   2,,drop=T]
     CatFull=s.@catch.n[ 2,,drop=T]
   	 Ufull  =s.@catch.wt[2,,drop=T]
  }else{
	   Ufull  =apply(i.@index[ -1,]*s.@catch.n[-1,],   2,sum)/apply(s.@catch.n[-1,],2,sum)
     CatFull=apply(s.@catch.n[-1,],                  2,sum)
   	 Wfull  =apply(s.@stock.wt[ -1,]*s.@catch.n[-1,],2,sum)/apply(s.@catch.n[-1,],2,sum)}

  data<-  list(Year   =as.integer(dimnames(s.@catch.n)$year),
               CatRec =s.@catch.n[1,,drop=T],
               CatFull=CatFull,
               Urec   =i.@index[1,,drop=T],
               Ufull  =Ufull,
               Wrec   =s.@stock.wt[1,,drop=TRUE],
               Wfull  =Wfull,
#              M      =apply(s.@m[    ,,drop=T],2,mean),
               M      =s.@m[    ,,drop=T],
               Srat   =rep(qratio,length(s.@catch.n[1,,drop=T])),
			         CatchN =apply(s.@catch.n,2,sum),
			         range  =c(minyear=dims(s.)$minyear,maxyear=dims(s.)$maxyear))

   return(data)
   }
