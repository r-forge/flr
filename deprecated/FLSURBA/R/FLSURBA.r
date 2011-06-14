  ### FL  R core version 0.7-0 ###
  ### FLSURBA
  ### This file contains code just for the core class FLSURBA

  ### 30-10-2004 implemented as a S4 Class L T Kell
  ### based upon original Fortran code by Coby Needle


  ### class ##############################################################
  validFLSURBA.control <- function(object){
  	# Everything is fine
  	return(TRUE)
  	}

  setClass("FLSURBA.control",
  	representation(
    	fbar              ="numeric",
  		refage            ="integer",
  		z.spwn            ="FLQuant",
  		smooth            ="numeric"),
  	prototype=prototype(
  		fbar              =c(min=as.numeric(NA),max=as.numeric(NA)),
  		refage            =as.integer(NA),
  		z.spwn            =as.FLQuant(0),
      smooth            =as.double(2.0)),
  	validity=validFLSURBA.control
  	)

  setValidity("FLSURBA.control", validFLSURBA.control)
  remove(validFLSURBA.control)	# We do not need this function any more


  ## FLSURBA ############################################################
  validFLSURBA <- function(object){
  	# All FLQuant objects must have same dimensions
  	Dim <- dim(object@n)
  	if (!all(dim(object@f) == Dim))
  		return("n and f arrays must have same dimensions")
  	# Everything is fine
  	return(TRUE)
  	}

  setClass("FLSURBA",
    contains="FLAssess",
  	representation(
  		desc        ="character",
  		call        ="character",
  		control     ="FLSURBA.control",
  ##	param		    ="numeric",		## Parameters
  ##	param.se	  ="numeric",		## S.E. of parameters
  		error		    ="character",	## error flag
  ##  ssb			    ="FLQuant",		## SSB
  ##	tsb			    ="FLQuant",		## TSB
  		z    	      ="FLQuant",		## Mean Z
  		z.bar	      ="FLQuant",		## Mean Z
  		z.var		    ="FLQuant",		## Variance of total mortality
  		z.varbar	  ="FLQuant",		## Variance of mean Z
  		r.var		    ="FLQuant",		## Variance of recruitment
  		rss  		    ="numeric"		## Variance of recruitment
  ##	ssb.var		  ="FLQuant",		## Variance of SSB
  ##	tsb.var		  ="FLQuant" ,	## Variance of TSB
  ),
  	prototype=prototype(
  		fbar        =as.numeric(NA),
  		desc        =character(0),
  		call        ="new(\"FLSURBA\")",
  		control     =new("FLSURBA.control"),
  		z.spwn      =as.FLQuant(NA),
  ##	param		    =as.numeric(NA),
  ##	param.se	  =as.numeric(NA),
  		error		    =character(0),
  		stock.n     =as.FLQuant(NA),
  		z			      =as.FLQuant(NA),
  ##	ssb			    =as.FLQuant(NA),
  ##	tsb			    =as.FLQuant(NA),
  		z.bar		    =as.FLQuant(NA),
  		z.var		    =as.FLQuant(NA),
  		z.varbar	  =as.FLQuant(NA),
  		rvar	  	  =as.FLQuant(NA),
  		rss	    	  =as.numeric(NA)
  ##	ssb.var		  =as.FLQuant(NA),
  ##	tsb.var		  =as.FLQuant(NA)
  ),
  	validity=validFLSURBA
  	)

  setValidity("FLSURBA", validFLSURBA)
  remove(validFLSURBA)	# We do not need this function any more
  ### End class ###########################################################


    ### Methods #############################################################
    FLSURBA <- function(stock="missing", indices="missing", control=FLSURBA.control(), desc=""){
    	Call <- match.call()

      ## check that stock is supplied
    	if (missing(stock))
    		stop("must supply stock")
    	if (!inherits(stock, "FLStock"))
    		stop("stock must be an 'FLStock' object!")
    	if (!validObject(stock))
    		stop("stock is not valid!")

      ## check at least one index is supplied, may include biomass index
    	if (missing(indices))
    		stop("must supply index")
      if (is(indices, "FLIndex"))
        indices <- FLIndices(indices)
      if (!inherits(indices, "FLIndices"))
    		stop("indices must be an 'FLIndices' object!")
    	if (!validObject(indices))
    		stop("indices not valid!")

      for (i in 1:length(indices)){
        if (all(is.na(indices[[i]]@index.q[])))   stop(paste("index.q all NA for index",i))
        if (all(is.na(indices[[i]]@index.var[]))) stop(paste("index.var all NA for index",i))
        }

      if (missing(control))	{
      	control@z.spwn<-(stock@harvest.spwn+stock@m.spwn)/2
      	}

      ## check control
      if (!inherits(control, "FLSURBA.control"))
    	  stop("control must be an 'FLSURBA.control' object!")

      ##check range
      temp<-unlist(lapply(indices, function(x) x@range))

      ## set FLIndices ranges and trim
      for (i in 1:length(FLIndices))
         {
         if(!is.na(indices[[i]]@range["plusgroup"]))
            indices[[i]]@range["max"]<-min(indices[[i]]@range["min"],indices[[i]]@range["plusgroup"]-1)

         indices[[i]]@range["min"]     <-max(stock@range["min"],     min(indices[[i]]@range["min"]))
         indices[[i]]@range["max"]     <-min(stock@range["max"],     min(indices[[i]]@range["max"]))
         indices[[i]]@range["minyear"] <-max(stock@range["minyear"], min(indices[[i]]@range["minyear"]))
         indices[[i]]@range["maxyear"] <-min(stock@range["maxyear"], min(indices[[i]]@range["maxyear"]))

         indices[[i]]<-trim(indices[[i]],age=indices[[i]]@range["min"]:indices[[i]]@range["max"],year=indices[[i]]@range["minyear"]:indices[[i]]@range["maxyear"])
         }

      stock@range["min"]    <-max(stock@range["min"],     min(temp[names(temp)=="min"]))
      stock@range["max"]    <-min(stock@range["max"],     max(temp[names(temp)=="max"]))
      stock@range["minyear"]<-max(stock@range["minyear"], min(temp[names(temp)=="minyear"]))
      stock@range["maxyear"]<-min(stock@range["maxyear"], max(temp[names(temp)=="maxyear"]))

      ## standardise surveys
      indices<-FLIndices(lapply(indices, function(x) {x@index<-x@index/mean(x@index,na.action=na.rm); return(x)}))

    	## fbar age range
      if (!any(names(control@fbar)=="min")) control@fbar["min"] <- stock@range["min"]
      if (!any(names(control@fbar)=="max")) control@fbar["max"] <- stock@range["max"]

      if (is.na(control@refage)) control@refage <- as.integer((stock@range["min"]+stock@range["max"])/2)

      if (!validObject(control))
          stop("control is not valid!")

      stock<-truncate.FLQuants(stock)
      for (i in 1:length(indices))
         indices[[i]]<-truncate.FLQuants(indices[[i]])

      ##n-at-age indices
      ##aggregate boiomass indices

      ## seperate biomass & age indices
      index.age<-new("FLIndices")
      index.ssb<-new("FLIndices")
      j<-1
      k<-1
      for (i in 1:length(indices))
        if (substr(indices[[i]]@type,1,1)=="b")
           {
           index.ssb[[j]]<-indices[[i]]
           j<-j+1
           }
        else
           {
           index.age[[k]]<-indices[[i]]
           k<-k+1
           }

      ## fix: create FLQuant with unit dim for indices  to pass to Fortran via .Call
      if (length(index)<1) stop("need at least 1 index by age")
      index.n.quant  <-create.indices(index.age,"index",     stock@range)
      index.q.quant  <-create.indices(index.age,"index.q",   stock@range)
      index.var.quant<-create.indices(index.age,"index.var", stock@range)
      index.rho      <-numeric(length(index.age))

      ## set rho; time of survey
      for (i in 1:length(indices))
             if (substr(indices[[i]]@type,1,1)=="n") index.rho[i]<-(index.age[[i]]@range["startf"]+index.age[[i]]@range["endf"])/2.0

      ## truncate
      index.n.quant
      ages<-as.character(max(dims(index.n.quant)$min,stock@range["min"]):min(dims(index.n.quant)$max,stock@range["max"]))
      yrs <-as.character(max(dims(index.n.quant)$minyear,stock@range["minyear"]):min(dims(index.n.quant)$maxyear,stock@range["maxyear"]))

      index.n.quant  <-index.n.quant[ages,yrs,,,]
      index.q.quant  <-index.q.quant[ages,yrs,,,]
      index.var.quant<-index.var.quant[ages,yrs,,,]

      ## Biomass index optional
      range    <-stock@range
      range[c("min","max")]<-c(1,1)
      if (length(index.ssb)>0){
         bindex.n.quant  <-create.indices(index.ssb,"index",     range, aggregated=TRUE)
         bindex.var.quant<-create.indices(index.ssb,"index.var", range, aggregated=TRUE)
         bindex.rho      <-numeric(length(index.ssb))
         for (i in 1:length(index.ssb))
            bindex.rho[i]<-(index.ssb[[i]]@range["startf"]+index.ssb[[i]]@range["endf"])/2.0

         ## truncate
         bindex.n.quant  <-bindex.n.quant[,yrs,,,]
         bindex.var.quant<-bindex.var.quant[,yrs,,,]
         }
      else
         {
         bindex.n.quant  <-list(NULL)
         bindex.var.quant<-list(NULL)
         bindex.rho      <-list(NULL)
         }

      if (is.na(stock@range["plusgroup"])) stock@range["plusgroup"]<--1

      #Run
      res <- .Call("__FLSURBA", stock,
                                index.n.quant, index.q.quant, index.var.quant, index.rho,
                                bindex.n.quant,               bindex.var.quant,bindex.rho,
                                control, c(stock@range,fbar.min=as.integer(control@fbar["min"]),fbar.max=as.integer(control@fbar["max"])))

    	res2         <-new("FLSURBA")
      res2@call    <- as.character(Call)
    	res2@control <- control
    	res2@stock.n <- FLQuant(res@stock.n,dimnames=dimnames(res@stock.n))
    	res2@z       <- FLQuant(res@z,      dimnames=dimnames(res@z))
    	units(res2@z)<- "f"

      res2@z.varbar<-FLQuant(res@z.varbar)
      res2@z.bar   <-FLQuant(res@z.bar)
      res2@z.var   <-FLQuant(res@z.var)
      res2@r.var   <-FLQuant(res@r.var)

      dmns<-dimnames(res2@z.bar)
      dmns$age<-"all"
      dimnames(res2@z.varbar)<-dmns
      dimnames(res2@z.bar)   <-dmns
      dimnames(res2@z.var)   <-dmns
      dimnames(res2@r.var)   <-dmns
      ##names(res@ssb.var) <-dmns
      ##names(res@tsb.var) <-dmns

      ## recombine ssb & age indices
      res2@index     <-new("FLQuants")
      res2@index.res <-new("FLQuants")
      res2@index.hat <-new("FLQuants")
      for (i in 1:length(index))
         {
         if (substr(indices[[i]]@type,1,1)=="n")
            ages <-as.character(indices[[i]]@range["min"]:indices[[i]]@range["max"])
         else
            ages <-1

         years<-as.character(indices[[i]]@range["minyear"]:indices[[i]]@range["maxyear"])

         #res2@index[[i]]    <-index[[i]]@index
         #res2@index.res[[i]]<-FLQuant(res@index.res[[i]],dimnames=dimnames(res@index.res[[i]]))
         #res2@index.hat[[i]]<-FLQuant(res@index.hat[[i]],dimnames=dimnames(res@index.hat[[i]]))

         res2@index[[i]]    <-indices[[i]]@index[ages,years]
         res2@index.res[[i]]<-as.FLQuant(res@index.res[[i]]) #[ages,years]
         res2@index.hat[[i]]<-FLQuant(res@index.hat[[i]]) #[ages,years]
         }

    	if (!missing(desc)) res2@desc <- as.character(desc)

    	return(res2)
      }

  FLSURBA.control <- function(obj=NULL, smooth=2.0, refage=as.integer(NA), fbar=c(min=as.integer(NA),max=as.integer(NA)), z.spwn=FLQuant(0)){
    	if (is.null(obj))
    		res <- new("FLSURBA.control", smooth=smooth, fbar=fbar, refage=as.integer(refage), z.spwn=z.spwn)

    	# reuse an FLSURBA.control object embedded in an FLSURBA object
    	else if (is(obj, "FLSURBA"))
        res <- obj@control
    	else if (is(obj, "FLSURBA.control"))
        res <- obj
    	else
    			stop("obj must be an 'FLSURBA' or 'FLSURBA.control' object!")

  		# Verify that this object is valid
  		test <- validObject(res)
  		if (!test)
  			stop("Invalid object:", test)

      # redefine some of its parameters
      res@refage <-as.integer(refage)
  	  res@smooth <-smooth
  		res@z.spwn <-z.spwn

  	return(res)
  }

  # Creator of FLSURBA objects, FLSURBA() is defined in Calls.R

  # Test if an object is of FLSURBA class
  is.FLSURBA <- function(x)
  	return(inherits(x, "FLSURBA"))

  # Test if an object is of FLSURBA.control class
  is.FLSURBA.control <- function(x)
  	return(inherits(x, "FLSURBA.control"))

  ## show (a replacement for print of S3 classes)
  setMethod("show", signature(object="FLSURBA.control"),
  	function(object){
        n.<-slotNames(object)
  	   for (i in 1:length(n.))
           cat(n.[i],"\t\t",slot(object,n.[i]),"\n")
  	}
  )

  ### End methods ################################################################


  ### Utility functions that are not exported in namespace #######################
  check.range<-function(obj, range, range2=c(min=NA,max=NA,minyear=NA,maxyear=NA),aggregated=FALSE){

       ## should only have 1 element
       if (dims(obj)$unit  !=1) stop("only valid for 1 unit")
       if (dims(obj)$area  !=1) stop("only valid for 1 area")
       if (dims(obj)$season!=1) stop("only valid for 1 season")

       ## get age range
       if (!aggregated) {
           if (is.na(range2["min"]))
              range2["min"] <- range["min"]
           else
              range2["min"] <- min(range["min"], range2["min"])

           ## junk plusgroup
           if (any(names(range) == "plusgroup") & !is.na(range["plusgroup"]))
               range["max"] = min(range["max"], range["plusgroup"]-1)

           if (is.na(range2["max"]))
               range2["max"] <- range["max"]
           else
               range2["max"] <- max(range["max"], range2["max"])

           if (range["min"] < dims(obj)$min) stop("min in range less than data in quant")
           if (range["max"] > dims(obj)$max) {
              #print(obj)
              #print(dims(obj)$max)
              #print(range["max"])
              stop("max in range greater than data in quant")
              }
           ages<-as.character(range["min"]:range["max"])
           }
       else
           ages<-1


       ## get year range
       if (is.na(range2["minyear"]))
           range2["minyear"] <- range["minyear"]
       else
           range2["minyear"] <- min(range["minyear"], range2["minyear"])

       if (is.na(range2["maxyear"]))
           range2["maxyear"] <- range["maxyear"]
       else
           range2["maxyear"] <- max(range["maxyear"], range2["maxyear"])

       if (range["minyear"] < dims(obj)$minyear) stop("minyear in range less than data in quant")
       if (range["maxyear"] > dims(obj)$maxyear) stop("maxyear in range greater than data in quant")

       yrs <-as.character(range["minyear"]:range["maxyear"])

       return(list(obj=obj[ages,yrs],range=range2))
       }

  create.indices<-function(indices, slot, range=c(min=NA,max=NA,minyear=NA,maxyear=NA),aggregated=FALSE){

     n.indices<-length(indices)
     new      <-list(NULL)
     for (i in 1:n.indices) {
        res                     <-check.range(slot(indices[[i]], slot), indices[[i]]@range, range, aggregated)
        slot(indices[[i]], slot)<-res$obj
        range                   <-res$range
        }

     new<-FLQuant(NA,dimnames=list(age=as.character(range["min"]:range["max"]),year=as.character(range["minyear"]:range["maxyear"]),unit=1:n.indices,season="all",area="unique"))
     for (i in 1:n.indices) {
        dmns<-dimnames(slot(indices[[i]], slot))
        if (substr(indices[[i]]@type,1,1)=="n") dmns.age<-dmns$age else dmns.age<-1
        new[dmns.age, dmns$year,i,,]<-slot(indices[[i]], slot)[dmns.age, dmns$year,1,,]
        }

    return(new)
    }

  dimnames.from.range<-function(range){
   list(age   =as.character(range["min"]:range["max"]),
        year  =as.character(range["minyear"]:range["maxyear"]),
        unit  ="unique",
        season="all",
        area  ="unique")
        }

  contains<-function(x, y) {
        for (i in x) if (!any(i == y)) return(FALSE)

        return(TRUE)
        }


  truncate.FLQuants<-function(x,range="missing"){

        if (missing(range))
           if ("range" %in% slotNames(x)) range<-x@range else stop("x must have a range slot or else range arg must be supplied")

        if (is.na(range["min"]) | is.na(range["max"]))
          ages<-NA
        else
          ages<-as.character(range["min"]:range["max"])

        if (is.na(range["minyear"]) | is.na(range["maxyear"]))
          yrs<-NA
        else
          yrs <-as.character(range["minyear"]:range["maxyear"])

        slots. <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])

        for (s. in slots.) {
           if (!all(is.na(ages))) if (contains(ages,dimnames(slot(x, s.))$age))
                          slot(x, s.) <- slot(x, s.)[ages,,,,]
           if (!all(is.na(yrs))) if (contains(yrs,dimnames(slot(x, s.))$year))
                          slot(x, s.) <- slot(x, s.)[,yrs,,,]
                           }
        return(x)
        }

  setMethod("assess", signature(control="FLSURBA.control"),
     function(control,stock,indices,...){

     if (!is(stock,   "FLStock"))   stop("stock not of type FLStock")
     if ( is(indices, "FLIndex"))   indices<-FLIndices(list(indices))
     if (!is(indices, "FLIndices")) stop("indices not of type FLIndices")

     print("FLSURBA")
     surba<-FLSURBA(stock,indices,object)

  #   return(list(surba@harvest,surba@z,stock@m[dimnames(surba@z)$age,dimnames(surba@z)$year]))
     surba@harvest<-surba@z-stock@m[dimnames(surba@z)$age,dimnames(surba@z)$year]
     units(surba@harvest)<-"f"
     return(surba)
     }
  )

  ### End utility functions ######################################################


  setMethod("update", signature(object="FLSURBA"),
        function(object, stock, ...){

      object.range<-dims(object@z)
      ages        <-object.range$min:object.range$max
      yrs         <-object.range$minyear:object.range$maxyear
      slots.      <-getSlots("FLStock")

      for (i in names(slots.[slots.=="FLQuant"]))
          {
          if (!is.na(dims(slot(stock,i))$min) && !is.na(dims(slot(stock,i))$max))
            slot(stock,i)<-trim(slot(stock,i),age=ages)

          slot(stock,i)<-trim(slot(stock,i),year=yrs)
          }

      stock@harvest <-object@z-stock@m
      stock@stock.n <-object@stock.n

      return(stock)
      })


  ## "+"      {{{
  setMethod("+", signature(e1="FLStock",e2="FLSURBA"),
  	function(e1, e2) {
          return(update(e2,e1))
      }
  )

  setMethod("+", signature(e2="FLSURBA",e1="FLStock"),
  	function(e1, e2) {
          return(update(e2,e1))
      }
  )
