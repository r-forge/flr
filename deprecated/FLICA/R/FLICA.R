.packageName <- "FLICA"
### FLR core version 0.7-0 ###
### FLICA
### This file contains code just for the core class FLICA

validFLICA.control <- function(object){
	if (any(object@lambda.age < 0))
		return("weights must be > 0")
	if (any(object@lambda.yr < 0))
		return("value of rage must be >= -1")
	if (object@lambda.sr < 0)
		return("SR weight must be >= 0")
	if (!is.na(object@sr.age))
  	if (object@sr.age < 0)
	   	return("sr.age must be >= 0")

	# Everything is fine
	return(TRUE)
}

setClass("FLICA.control",
  contains="FLSepVPA.control",
	representation(
    sep.2  		   ="integer",   ## Year at which to change selection pattern, if NA only one selection pattern
		sep.gradual  ="logical",   ## whether change in selection is gradual or step
		sr			     ="logical",   ## Stock and recruitment parameters, i.e. whether to fit a stock-recruit relation
		sr.age       ="integer",   ## age at recruitment
		lambda.age	 ="numeric",   ## Weighting matrices for catch-at-age; for aged surveys; for SSB surveys
		lambda.yr	   ="numeric",   ## Relative weights by year
		lambda.sr	   ="numeric",   ## weight for the SRR term in the objective function
		index.model  ="character", ## 'p' N=aU^b, 'l' N=aU, 'a' N = U
		index.cor    ="numeric"),  ## Are the age-structured indices are correlated across ages
	prototype=prototype(
    sep.2  		   =as.integer(NA),
		sep.gradual  =as.logical(TRUE),
		sr			     =as.logical(TRUE),
		sr.age  	   =as.integer(NA),
		lambda.age	 =as.numeric(1.0),
		lambda.yr	   =as.numeric(1.0),
		lambda.sr    =as.numeric(0.1),
		index.model  =as.character("l"),
		index.cor    =as.numeric(0.0)),
	validity=validFLICA.control
)

setValidity("FLICA.control", validFLICA.control)
remove(validFLICA.control)	# We do not need this function any more


## FLICA #######################################################################
validFLICA <- function(object){
	# All FLQuant objects must have same dimensions
	Dim <- dim(object@stock.n)
	if (!all(dim(object@harvest) == Dim))
		return("n and f arrays must have same dimensions")
	# Everything is fine
	return(TRUE)
}

  setClass("FLICA",
    contains="FLAssess",
  	representation(
      call      ="character", 
      catch.res ="FLQuant",    #residuals from seperable analysis log(catch.n/catch.hat)
      param     ="data.frame",      #parameters
      sum.sq   ="data.frame",  #mean square weights
      survivors  ="FLQuant",     #Stock.n estimates in the terminal year
      covar     ="array",      #covar
      q         ="FLQuant",    #catchability by index & age
      q.var     ="FLQuant",    #catchability var by index & age
      f.ref     ="FLQuant",    #the estimate of F for the reference age in the separable model
      f.ref.var ="FLQuant",    #the standard errors about the reference age in F for the reference age
      ssb.var   ="FLQuant",    #the standard errors around the estimate of SSB in the separable periods
      sel       ="FLQuant",    #the selection pattern for the separable period by age
      sel.var   ="FLQuant",    #the standard error of that selection pattern by age
      ica.out   ="character",   #the ica.out documentation file
      resids    ="FLQuants",    #all the residuals from the together in one place
      weighted.resids    ="FLQuants",    #all the weigthed residuals from the together in one place
  		control  ="FLICA.control"),
  	prototype=prototype(
      call      =new("character"), 
      catch.res =new("FLQuant"),
      param     =new("data.frame"),
      sum.sq   =new("data.frame"),
      survivors =new("FLQuant"),
      covar     =new("array"),
      q         =new("FLQuant"),
      q.var     =new("FLQuant"),
      f.ref     =new("FLQuant"),
      f.ref.var =new("FLQuant"),
      ssb.var   =new("FLQuant"),
      sel       =new("FLQuant"),
      sel.var   =new("FLQuant"),
      ica.out   =new("character"),
      resids    =new("FLQuants"),
      weighted.resids=new("FLQuants"),
  		control  =new("FLICA.control")),
  	validity=validFLICA
  )

setValidity("FLICA", validFLICA)
remove(validFLICA)	# We do not need this function any more

### End class ##################################################################


### Methods ####################################################################
FLICA <- function(FLStock, FLIndices, control="missing", desc){
	Call <- match.call()
	if (!inherits(FLStock, "FLStock"))
		stop("FLStock must be an 'FLStock' object!")
	if (!inherits(FLIndices, "FLIndices"))
		stop("FLIndices must be an 'FLIndices' object!")
	if (!validObject(FLStock))
		stop("FLStock is not valid!")
	if (!validObject(FLIndices))
		stop("FLIndices is not valid!")
    if (missing(control))
		control<-FLICA.control()
	else if (!validObject(control))
		stop("control is not valid!")

  if (dims(FLStock)$iter>1) stop("FLICAcurrently cannot handle more than 1 iter in FLStock")
  if (dims(FLStock)$area>1) stop("FLICA currently cannot handle more than 1 area in FLStock")
  if (dims(FLStock)$season>1) stop("FLICA currently cannot handle more than 1 season in FLStock")
  if (dims(FLStock)$unit>1) stop("FLICA currently cannot handle more than 1 unit in FLStock")
  if (is.na(FLStock@range["plusgroup"])) stop("FLICA needs the plusgroup in stock object to be set.")

  ##check index types
  get.type<-function(index) return(index@type)
  types<-lapply(FLIndices,get.type)
  if (!all(types %in% c("biomass","number")))
     stop("FLIndex type must be 'biomass' or 'number'")

   for (i in 1:length(FLIndices))
      if (any(is.na(FLIndices[[i]]@index.var))) stop(paste("NAs in 'index.var' slot of FLIndex ", i, "; change to +ve value",sep=""))

   ##check that there are not multiple iters
   chk.dims<-function(obj)
      {
      dms<-dim(slot(obj,"index"))

      if (length(dms)>5 && dms[6]>1) return(FALSE) else return(TRUE)
      }
   if (!all(unlist(lapply(FLIndices,chk.dims)))) stop("more than 1 iter in FLIndices")


  ##check that if any biomass indices provided that they all start and end in same years
  dim.minyear<-function(index) dims(index)$minyear
  dim.maxyear<-function(index) dims(index)$maxyear
  if (any(lapply(FLIndices,get.type)=="biomass"))
     if ((length(unique(lapply(FLIndices[(1:length(FLIndices))[unlist(lapply(FLIndices,get.type))=="biomass"]],dim.minyear)))>1) &
         (length(unique(lapply(FLIndices[(1:length(FLIndices))[unlist(lapply(FLIndices,get.type))=="biomass"]],dim.maxyear)))>1))
         stop("biomass indices must all start and end in same years")

  check.for.na <- function(obj) {
       if (all(is.na(obj@index.var))) {
          warning("replacing NAs with 1.0 in index.var")
          obj@index.var[]<-1.0
          }
       }
  lapply(FLIndices,check.for.na)

#MPA, 27/02/2008: Commented out, as res is not defined until later
#	if (!missing(desc)) res@desc <- as.character(desc)

	if (!inherits(control, "FLICA.control"))
		stop("control must be an 'FLICA.control' object!")
	else if (missing(control))
		control<-FLICA.control(FLStock)

  if (length(control@index.model)>length(FLIndices))
    stop("more index.model's specified then FLIndex's")
  else if (length(control@index.model)==1 && length(FLIndices)>1)
    {
    len<-length(control@index.model)
    control@index.model<-c(control@index.model,rep(control@index.model[len],length(FLIndices)-len))
    }

  if (length(control@index.cor)>length(FLIndices))
    stop("more index.model's specified then FLIndex's")
  else if (length(control@index.cor)==1 && length(FLIndices)>1)
    {
    len<-length(control@index.cor)
    control@index.cor<-c(control@index.cor,rep(control@index.cor[len],length(FLIndices)-len))
    }
  if (is.na(control@sr.age)) control@sr.age<-as.integer(dims(FLStock@catch.n)$min)
  if (length(control@lambda.age) < dims(FLStock@catch.n)$age)
       control@lambda.age<-c(control@lambda.age,rep(control@lambda.age[length(control@lambda.age)],dims(FLStock@catch.n)$age-length(control@lambda.age)))
  if (length(control@lambda.yr) < control@sep.nyr)
       control@lambda.yr<-c(control@lambda.yr,rep(control@lambda.yr[length(control@lambda.yr)],control@sep.nyr-length(control@lambda.yr)))
  if (control@sep.age>=dims(FLStock@catch.n)$max) control@sep.age<-as.integer((dims(FLStock@catch.n)$min+dims(FLStock@catch.n)$max)/2)

	res <- .Call("FLICA", FLStock, FLIndices, control)
	res@call <- as.character(Call)
	if (!missing(desc)) res@desc <- as.character(desc)
	res2           <- new("FLICA")

#	res2@stock.n   <- FLQuant(res@stock.n@.Data, dimnames=dimnames(res@stock.n@.Data))
#	res2@harvest   <- FLQuant(res@harvest@.Data, dimnames=dimnames(res@harvest@.Data))
#	res2@catch.n   <- FLQuant(res@catch.n@.Data, dimnames=dimnames(res@catch.n@.Data))
#	units(res2@harvest) <- "f"
#
    #MPA: 11 March 2009  - Kludge to make the dimnames propigate properly
	res2@stock.n   <- FLQuant(res@stock.n@.Data, dimnames=dimnames(FLStock@stock.n@.Data))
	res2@harvest   <- FLQuant(res@harvest@.Data, dimnames=dimnames(FLStock@harvest@.Data))
	res2@catch.n   <- FLQuant(res@catch.n@.Data, dimnames=dimnames(FLStock@catch.n@.Data))
	units(res2@harvest) <- "f"

  ## selection pattern
  yrs            <-as.character((dims(res2@harvest)$maxyear-max(control@sep.nyr)+1):dims(res2@harvest)$maxyear)
  res2@sel       <-sweep(res2@harvest[,yrs],2,res2@harvest[as.character(control@sep.age),yrs],"/")
  res2@catch.res <-log(FLStock@catch.n[,yrs]/res2@catch.n[,yrs])

  res2@f.ref     <-res2@harvest[as.character(control@sep.age),yrs]

	res2@control   <- res@control
	if  (res@control@sr != control@sr) warning("SR parameters were -ve so sr option set to false in control")

	res2@index     <-  FLQuants(lapply(res@index,     function(x) as.FLQuant(x,dimnames=dimnames(as.array(x)))))
	res2@index.res <-  FLQuants(lapply(res@index.res, function(x) as.FLQuant(x,dimnames=dimnames(as.array(x)))))
#	res2@index.hat <-  FLQuants(lapply(res@index.hat, function(x) as.FLQuant(x,dimnames=dimnames(as.array(x)))))

	res2@call      <- res@call
  res2@desc      <- res@desc
  res2@covar     <- res@covar

  #MPA, 13/03/08: Setup params slot, with params and error. Most parameters are log transformed in the
  #output from the compiled code - here we correct this. Add in other items - cv, confidence intervals
  params.array     <- cbind(res@param,sqrt(diag(res@covar)),exp(res@param),NA,NA)
  colnames(params.array)    <-  c("ICA.Output","Std.dev","Value","Lower.95.pct.CL","Upper.95.pct.CL")
  params.array[,"Lower.95.pct.CL"]      <-  exp(params.array[,"ICA.Output"] - 1.96*params.array[,"Std.dev"])
  params.array[,"Upper.95.pct.CL"]      <-  exp(params.array[,"ICA.Output"] + 1.96*params.array[,"Std.dev"])
  res2@param                            <-  as.data.frame(params.array)

  #MPA, 16/03/08: Setup lists to catch the extra information in the params slot
  param.name.l  <-  year.l  <- age.l  <-  index.l <-  list()

  #MPA, 29/02/08: Calculate index.hat - the values returned by the compiled code give the same value for
  #index.hat and index.res, so here we recalculate index.hat from index and index.res
  for (i in 1:length(FLIndices)){
     res2@index.res[[i]][FLIndices[[i]]@index<0 | !is.finite(FLIndices[[i]]@index)]<-NA
     res2@index[[i]][    FLIndices[[i]]@index<0 | !is.finite(FLIndices[[i]]@index)]<-NA
  }

	#MPA, 8/03/08: Index is returned as log transformed. Correct this so that it makes some sense
  for(i in 1:length(res2@index)) {
      res2@index[[i]] <-  exp(res2@index[[i]])
  }

  #MPA, 7/02/08: Had neglected the SIMPLIFY=FALSE option, which caused problems when only one input index!
  index.hat.l     <-  mapply(res2@index,res2@index.res, SIMPLIFY=FALSE,
                          FUN=function(idx,resids){exp(log(idx)-resids)})
	res2@index.hat  <-  do.call(FLQuants,index.hat.l)

  #MPA, 16/08/08: Calculate the residuals for absolute indices
  for(i in 1:length(FLIndices)) {
    if(res2@control@index.model[i] =="absolute") {
      res2@index[[i]] <- FLIndices[[i]]@index
      #As we're dealing with an absolute index, index.hat => estimated stock numbers
      if(FLIndices[[i]]@type =="biomass") {
          res2@index.hat[[i]] <- ssb(res2+FLStock)
      } else if(FLIndices[[i]]@type =="number") {
          res2@index.hat[[i]] <- res2@stock.n
      }
      #Trim index.hat to the appropriate size
      trim.args       <-  c(object=list(res2@index.hat[[i]]),dimnames(FLIndices[[i]]@index))
      trim.args$iter  <- NULL   #This is currently only a 5D quant...
      res2@index.hat[[i]] <- do.call(trim,trim.args)
      #Calculate residuals
      res2@index.res[[i]] <- log(res2@index[[i]]/res2@index.hat[[i]])
    }
  }


  #MPA, 29/02/08: Add information about the index to outputs- for later use by the FLICA diagnostics plots
  for (i in 1:length(FLIndices)){
     attr(res2@index[[i]],"type")     <-  FLIndices[[i]]@type
     res2@index.range[[i]]            <-  FLIndices[[i]]@range
  }

  #MPA, 29/02/08: Add names to the indices lists
  idx.names            <-  names(FLIndices)
  if(all(is.na(idx.names) | idx.names=="")) idx.names <-  sapply(FLIndices,name)    #Copy names from individual FLIndex objects
  names(res2@index)       <-  idx.names
  names(res2@index.res)   <-  idx.names
  names(res2@index.hat)   <-  idx.names
  names(res2@index.range) <-  idx.names

  #MPA, 10/02/09: Add index.variances to output
  res2@index.var <- lapply(FLIndices,index.var)

  #MPA, 10/02/09: Populate the weighted.res and unweighted.res slots
  res2@resids    <- do.call(FLQuants,c(list(Catch=res2@catch.res),res2@index.res))
  catch.wts      <- FLQuant(res2@control@lambda.age %o% res2@control@lambda.yr,dim=dim(res2@catch.res@.Data))
  catch.weighted <- res2@catch.res*sqrt(catch.wts)
  idx.weighted   <- mapply(res2@index.res,res2@index.var,SIMPLIFY=FALSE,FUN=function(idx.res,idx.var) {idx.res/sqrt(idx.var)})   #Inverse variance weighting
  res2@weighted.resids <- do.call(FLQuants,c(list(Catch=catch.weighted),idx.weighted))

  #MPA, 18/03/08: Calculation of mean sqs - this feature is still under development
  unwt.index.sqs <-  sapply(res2@index.res,function(idx.res) {sum(idx.res^2,na.rm=TRUE)})
  unwt.catch.sqs <-  sum(res2@catch.res^2,na.rm=TRUE)
  wt.index.sqs   <-  mapply(res2@index.res,FLIndices,SIMPLIFY=TRUE,FUN=function(idx.res,idx) {
                        sum((idx.res/idx@index.var)^2,na.rm=TRUE) })
  wt.catch.sqs   <-  sum(sweep(res2@catch.res,1,res2@control@lambda.age,"*")^2,na.rm=TRUE)
  res2@sum.sq  <-  data.frame(Weighted=c(wt.catch.sqs,wt.index.sqs,sum(wt.catch.sqs,wt.index.sqs)),
                      Unweighted=c(unwt.catch.sqs,unwt.index.sqs,sum(unwt.catch.sqs,unwt.index.sqs)))
  rownames(res2@sum.sq)  <- c("Catch",idx.names,"Total SSQ")

  #MPA, 21/03/09: set ica range
  res2@range <- range(FLStock)[1:5]

  ############################################
  ## Parameter names
  ## Created by : Hans Bogard March 2007
  ## Modified : Sarah Clarke December 2007
  ## Further development and incorporation into FLICA: Mark Payne, February, March 2007
  ############################################
  # Determine parameter names
    parmno     <- 0
    param.names<-character(0)
    ## Separable model parameters: F by year
    for (year in (FLStock@range["maxyear"]-res2@control@sep.nyr+1):(FLStock@range["maxyear"])){
      parmno <- parmno+1
      param.names[parmno]<-paste("F,",year)
      param.name.l[parmno]  <-  "F"
      year.l[parmno]        <-  year
    }

    ## Separable model parameters: Selection by age
    for (age in (FLStock@range["min"]:(FLStock@range["max"]-1)))
      if ((age != res2@control@sep.age) && (age != FLStock@range["plusgroup"]-1))
        {
          parmno <- parmno+1
          param.names[parmno]<-paste("Selectivity at age",age)
          param.name.l[parmno]  <-  "Sel"
          age.l[parmno]         <-  age
        }

  ## second selection pattern
    if (length(res2@control@sep.nyr)>1)
      for (age in FLStock@range["min"]:(FLStock@range["max"]-1))
        if ((age != res2@control@sep.age) && (age != FLStock@range["plusgroup"]-1))
          {
            parmno <- parmno+1
            param.names[parmno]<-paste("Period 2, Selectivity at age",age)
            param.name.l[parmno]  <-  "Sel2"
            age.l[parmno]         <-  age
          }

    ## Separable model parameters: Populations in the terminal year
    for (age in FLStock@range["min"]:(FLStock@range["max"]-1))
      {
        parmno=parmno+1
        param.names[parmno]<-paste("Terminal year pop, age",age)
        param.name.l[parmno]  <-  "TermN"
        age.l[parmno]         <-  age
        year.l[parmno]        <-  FLStock@range["maxyear"]
      }

    ##  Poplns at last true age in the years of the separable analysis
    for (year in (FLStock@range["maxyear"]-res2@control@sep.nyr+1):(FLStock@range["maxyear"]-1))
      {
        parmno=parmno+1
        param.names[parmno]<-paste("Last true age pop,",year)
        param.name.l[parmno]  <-  "OldN"
        year.l[parmno]        <-  year
        age.l[parmno]         <-  FLStock@range["plusgroup"]-1
      }

    #Recruitment prediction
    #This seems to be the source of some problems. In the FORTRAN code, the decision to do
    #recruitment prediction or not is based on the following code eg line 980 of ica2.f90 and following
      #      UseRecr = .false.
      #      do index= 1, nageix
      #       if ((fage(index).eq.firstage).and.(lyear(index).eq.lastyear+1))  &
      #             UseRecr = .true.
      #      enddo
    #Here, we interpret this as meaning that the recrutiment prediction is only done if there is
    #sufficient data ie one age structured numbers index must contain data for the recruits one year
    #ahead of the terminal year. Bogaards original criteria, "if (res2@control@sr)", therefore
    #seems to be based on an incorrect understanding of this parameter.
    can.pred.rec  <-  sapply(FLIndices,function(idx) {
                      if(idx@type=="number") {    #Only do it for number indicies
                          fage  <-  idx@range["min"]
                          lyear <-  idx@range["maxyear"]
                          first.age <-  FLStock@range["min"]
                          last.year <-  FLStock@range["maxyear"]
                          return( (fage==first.age) & (lyear == (last.year+1)) )
                      } else {return(FALSE)}})
    if(any(can.pred.rec)){
        parmno <- parmno+1
        param.names[parmno]<-paste("Recruitment prediction")
        param.name.l[parmno]  <-  "Rec"
      }

    ##  SSB indices
    #Bogaards code seems to loop too many times here. Completely rewritten.
    Qparm <- res2@control@index.model
    for (i in 1:length(FLIndices)){
      if(FLIndices[[i]]@type=="biomass") {
        if (Qparm[i] == "linear")
          {
            parmno <- parmno+1
            param.names[parmno]<-paste("Index ", i,", biomass, Q",sep="")
            param.name.l[parmno]  <-  "Q"
            index.l[parmno]        <-  i
          }
        else if (Qparm[i] == "power")
          {
            #The correct order of these parameters is not completely clear, and it
            #appears that there is an error in the output in ica.out, where these labels are swapped.
            #K is returned as non-log transformed
            parmno <- parmno+1
            param.names[parmno]<-paste("Index ", i,", biomass, K",sep="")
            param.name.l[parmno]  <-  "K"
            index.l[parmno]        <-  i
            res2@param[parmno,"Value"]  <-  res2@param[parmno,"ICA.Output"]
            res2@param[parmno,"Lower.95.pct.CL"]  <-  res2@param[parmno,"ICA.Output"] - 1.96 * res2@param[parmno,"Std.dev"]
            res2@param[parmno,"Upper.95.pct.CL"]  <-  res2@param[parmno,"ICA.Output"] + 1.96 * res2@param[parmno,"Std.dev"]
            #Now the Catchability
            parmno <- parmno+1
            param.names[parmno]<-paste("Index ", i,", biomass, Q",sep="")
            param.name.l[parmno]  <-  "Q"
            index.l[parmno]        <-  i
          }
       } #End if statement
    }   #End for loop

    ## age-structured indices
    for (i in 1:length(FLIndices)) {
     if (FLIndices[[i]]@type=="number"){
        for (age in (FLIndices[[i]]@range["min"]:FLIndices[[i]]@range["max"])) {
           if (res2@control@index.model[i] == "linear"){
              parmno <- parmno+1
              param.names[parmno]<-paste("Index ",i,", age ",age," numbers, Q",sep="")
              param.name.l[parmno]  <-  "Q"
              index.l[parmno]        <-  i
              age.l[parmno]          <-  age
            }
           if (res2@control@index.model[i] == "power"){
              #K is returned as non-log transformed
              parmno <- parmno+1
              param.names[parmno]<-paste("Index ",i,", age ",age," numbers, K",sep="")
              res2@param[parmno,"Value"]  <-  res2@param[parmno,"ICA.Output"]
              res2@param[parmno,"Lower.95.pct.CL"]  <-  res2@param[parmno,"ICA.Output"] - 1.96 * res2@param[parmno,"Std.dev"]
              res2@param[parmno,"Upper.95.pct.CL"]  <-  res2@param[parmno,"ICA.Output"] + 1.96 * res2@param[parmno,"Std.dev"]
              param.name.l[parmno]   <-  "K"
              index.l[parmno]        <-  i
              age.l[parmno]          <-  age
              #Catchability
              parmno <- parmno+1
              param.names[parmno]    <-  paste("Index ",i,", age ",age," numbers, Q",sep="")}
              param.name.l[parmno]   <-  "Q"
              index.l[parmno]        <-  i
              age.l[parmno]          <-  age
        }
     }
    }

    ## stock recruitment relationship ##
    #Bogaard added this parameter based on "if (res2@control@sr)" ie the returned control object.
    #However, there are times when the stock recuitment parameters estimated are negative, and
    #in this case, the use of the sr function is switched off - however, dll still returns the
    #estimated values regardless, so basing this decision on the input control is more appropriate.
    if (control@sr){
        parmno<-parmno+1
        param.names[parmno]<-paste("SRR, a")
        param.name.l[parmno]  <-  "SRRa"
        parmno<-parmno+1
        param.names[parmno]<-paste("SRR, b")
        param.name.l[parmno]  <-  "SRRb"
    }

  # Now add pnames to the appropriate slots - but only if they match
  if(parmno == dim(res2@param)[1]) {
    age.l[parmno+1] <-  year.l[parmno+1] <- index.l[parmno+1] <- param.name.l[parmno+1] <- NULL
    ages  <-  sapply(age.l,function(i) {ifelse(is.null(i),"",i)})
    years <-  sapply(year.l,function(i) {ifelse(is.null(i),"",i)})
    indices  <-  sapply(index.l,function(i) {ifelse(is.null(i),"",i)})
    params  <-  sapply(param.name.l,function(i) {ifelse(is.null(i),"",i)})
    nos     <-  sequence(parmno)
    param.details  <-  data.frame(nos,params,years,ages,indices,stringsAsFactors=FALSE)
    colnames(param.details)   <-  c("No","Param","Year","Age","Index")
    res2@param              <-  data.frame(param.details,res2@param,stringsAsFactors=FALSE)
    rownames(res2@param)     <- param.names
    dimnames(res2@covar) <- list(param.names,param.names)
  } else {
    warning(paste("Number of parameters returned by FLICA dll (",dim(res2@param)[1],") does not match those estimated internally by R code (",length(param.names),").\n",sep=""))
    attr(res2@param ,"pnames")  <-  param.names
  }

  #MPA, 21/03/2009: Estimation of stock numbers in the terminal year
  maxyr         <-   dims(FLStock)$maxyear
  TY            <-   as.character(maxyr+1)
  maxage        <-   dims(FLStock)$max
  minage        <-   dims(FLStock)$min
  res2@survivors  <-  res2@stock.n[,ac(maxyr)]*exp(-res2@harvest[,ac(maxyr)]-FLStock@m[,ac(maxyr)])   #Stock.n at 31st December in maxyear
  dimnames(res2@survivors)$year <- TY
  #Now offset by one year, and add in recruitment estimate
  res2@survivors[ac(maxage),TY]                 <- res2@survivors[ac(maxage),TY] + res2@survivors[ac(maxage-1),TY]
  res2@survivors[ac((minage+1):(maxage-1)),TY]  <- res2@survivors[ac((minage):(maxage-2)),TY]
  res2@survivors[ac(minage),TY]                 <- NA
  rec.est   <-  subset(res2@param,res2@param$Param=="Rec")$Value
  if(length(rec.est)==1) res2@survivors[ac(minage),TY] <- rec.est
  units(res2@survivors) <- "NA"

  
  #Generate ica.out file
  res2@ica.out <- ica.out(FLStock+res2,FLIndices,res2)
  
	return(res2)
  }

FLICA.control <- function(FLICA=NULL,sep.nyr="missing",sep.age="missing",sep.sel ="missing",
									 sr="missing",sr.age="missing",
									 lambda.age="missing",lambda.yr="missing",lambda.sr="missing",
									 index.use="missing",index.model="missing",index.cor="missing"){

  if (is.null(FLICA))
		{
		res<-new("FLICA.control")
		}
	else if (inherits(FLICA, "FLICA"))
		{
        res<-FLICA
		}
	else if (inherits(FLICA, "FLStock"))
		{
    res                    <-FLICA.control()
		res@lambda.age		     <-as.numeric(rep(1,length(dimnames(FLICA@catch)[[1]])))
		names(res@lambda.age)  <-dimnames(FLICA@catch)[[1]]
		res@lambda.yr          <-as.numeric(rep(1.0,res@sep.nyr))
		res@sr.age             <-sr.age
		names(res@lambda.yr)   <-dimnames(her@catch)[[2]][(dim(her@catch)[2]-res@sep.nyr+1):dim(her@catch)[2]]
		}
	else
		stop("FLICA must be an 'FLICA' object!")

	if (!missing(sep.nyr))
		res@sep.nyr  <- as.integer(sep.nyr)
	if (!missing(sep.age))
		res@sep.age  <- as.integer(sep.age)
	if (!missing(sep.sel))
		res@sep.sel <- as.numeric(sep.sel)
	if (!missing(sr))
		res@sr       <- as.logical(sr)
	if (!missing(sr.age))
		res@sr.age   <- as.integer(sr.age)
	if (!missing(lambda.age))
		res@lambda.age   <- as.numeric(lambda.age)
	if (!missing(lambda.yr))
		res@lambda.yr    <- as.numeric(lambda.yr)
	if (!missing(lambda.sr))
		res@lambda.sr    <- as.numeric(lambda.sr)
  else
    res@lambda.sr<-0.1
	if (!missing(index.model))
		res@index.model   <- as.character(index.model)
	if (!missing(index.cor))
		res@index.cor    <- as.numeric(index.cor)
	if (!missing(index.use))
		res@index.use    <- as.logical(index.use)

	# Verify that this object is valid
	test <- validObject(res)
		if (!test)
			stop("Invalid object:", test)

	return(res)
}

## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLICA.control"),
	function(object){
      n.<-slotNames(object)
	   for (i in 1:length(n.))
         cat(n.[i],"\t\t",slot(object,n.[i]),"\n")
	}
)


# Test if an object is of FLICA class
is.FLICA <- function(x)
	return(inherits(x, "FLICA"))

# Test if an object is of FLICA.control class
is.FLICA.control <- function(x)
	return(inherits(x, "FLICA.control"))

setMethod("assess", signature(control="FLICA.control"),
   function(control,stock,indices,...){

   if (!is(stock,   "FLStock"))   stop("stock not of type FLStock")
   if ( is(indices, "FLIndex"))   indices<-FLIndices(list(indices))
   if (!is(indices, "FLIndices")) stop("indices not of type FLIndices")

   print("FLICA assessment method called")
   FLICA(stock,indices,control)
   }
)



#NxParm = 2*NySep-1 + (lastage-firstage) + (lastage-firstage-2)
#
# if (TwoSel) NxParm = NxParm+ (lastage-firstage-2)
#
#   do index = 1, nssbix
#      sum    = 0d0
#      count  = 0d0
#	  Nxparm = Nxparm+1
#	     DO year = fbyear, lbyear
#	        if (BSurvey(index, year-fbyear+1) .ge. 0.0 ) then
#	           sum = sum+dble(BSurvey(index, year-fbyear+1))
#	     enddo
#
#     # CHECK ENOUGH DEGREES OF FREEDOM
#     if (count .lt. QBParm(index)) then
#        return
#     endif
#
#     Bscale(index) = sum/count
#
#     nxdata = nxdata + idint(count)
#
#   enddo # indices
#
#   # Next do the age-structured indices
#
#      do index = 1, nageix
#	     do age = fage(index), lage(index)
#	        NxParm = Nxparm+1
#	        sum = 0d0
#	        count = 0d0
#	        DO year = fyear(index), lyear(index)
#	           iage = age-fage(index)+1
#	           if (ASurvey(index, year-fyear(index)+1,iage) .ne. missing) then
#		          sum =sum+dble(ASurvey(index, year-fyear(index)+1,iage))
#                  count = count+1d0
#	           endif
#	        enddo # years
#
#	        nxdata = nxdata + idint(count)
#	        Ascale(index) = sum/count
#         if (count .le. QAParm(index)) then
#			return
#		 endif
#	  enddo # ages
#   enddo # indices

#NxParm = 2*NySep-1 + (lastage-firstage) + (lastage-firstage-2)
#
# if (TwoSel) NxParm = NxParm+ (lastage-firstage-2)
#
#   do index = 1, nssbix
#      sum    = 0d0
#      count  = 0d0
#	  Nxparm = Nxparm+1
#	     DO year = fbyear, lbyear
#	        if (BSurvey(index, year-fbyear+1) .ge. 0.0 ) then
#	           sum = sum+dble(BSurvey(index, year-fbyear+1))
#	     enddo
#
#     # CHECK ENOUGH DEGREES OF FREEDOM
#     if (count .lt. QBParm(index)) then
#        return
#     endif
#
#     Bscale(index) = sum/count
#
#     nxdata = nxdata + idint(count)
#
#   enddo # indices
#
#   # Next do the age-structured indices
#
#      do index = 1, nageix
#	     do age = fage(index), lage(index)
#	        NxParm = Nxparm+1
#	        sum = 0d0
#	        count = 0d0
#	        DO year = fyear(index), lyear(index)
#	           iage = age-fage(index)+1
#	           if (ASurvey(index, year-fyear(index)+1,iage) .ne. missing) then
#		          sum =sum+dble(ASurvey(index, year-fyear(index)+1,iage))
#                  count = count+1d0
#	           endif
#	        enddo # years
#
# zzz - «Short one line description»

# Author: Iago Mosqueira, AZTI Fundazioa
# Additions:
# Last Change: 02 Ago 2005 13:33
# $Id: FLICA.R,v 1.24 2009/09/16 08:58:00 m_payne Exp $

# Reference:
# Notes:

# TODO Ven 22 Abr 2005 08:27:40 BST iagoazti:

#options(warn.FPU=FALSE)

.onLoad <- function(lib,pkg) {
	require(methods)
}




