

### class ######################################################################
 createFLAccesors <- function(object, exclude=character(1)) {

	slots <- getSlots(class(object))[!names(getSlots(class(object)))%in%exclude]

	defined <- list()

	for (x in names(slots)) {
		# check method is defined already and signatures match
		eval(
		substitute(if(isGeneric(x) && names(formals(x)) != "object") {warning(paste("Accesor
			method for", x, "conflicts with a differently defined generic. Type", x,
			"for more information")); break}, list(x=x))
			)
		# create new generic and accesor method
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x, function(object, ...) standardGeneric(x)),
		list(x=x))
		)
		eval(
		substitute(setMethod(x, signature(y), function(object) return(slot(object, x))), list(x=x,
			y=class(object)))
		)
		# create replacement method
		xr <- paste(x, "<-", sep="")
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x,
			function(object, ..., value) standardGeneric(x)), list(x=xr))
		)
		eval(
		substitute(setMethod(x, signature(object=y, value=v), function(object, value)
			{slot(object, s) <- value; object}), list(x=xr, y=class(object), s=x,
			v=unname(slots[x])))
		)
		defined[[x]] <- c(x, xr, paste('alias{',x,',',class(object),'-method}', sep=''),
			paste('\alias{',xr,',',class(object),',',unname(slots[x]), '-method}', sep=''),
			paste('\alias{',x,'-methods}', sep=''),
			paste('\alias{"',xr, '"-methods}', sep='')
		)
	}
	return(defined)
}	# }}}


validDynState.control <- function(object){

	if (object@Spp1Uplimit <= 0)
		return("value of Spp1Uplimit must be > 0")
	if (object@Spp2Uplimit <= 0)
		return("value of Spp2Uplimit must be > 0")
	if (object@EffortUplimit <= 0)
		return("value of EffortUplimit must be > 0")
	if (object@SimNumber <= 0)
		return("value of Simumber must be > 0")
  if (object@Increments <= 0)
		return("value of Increments must be > 0")
  # Everything is fine
	return(TRUE)
}

setClass("DynStateInput",
	representation(
        CatchMean  ="FLQuant",
        CatchSigma ="FLQuant"),
	prototype=prototype(
	      CatchMean  =FLQuant(),
        CatchSigma =FLQuant())
)

invisible(createFLAccesors(new("DynStateInput")))

setClass("DynState.control",
	representation(
        Spp1Uplimit      ="numeric",
        Spp2Uplimit      ="numeric",
        EffortUplimit    ="integer",
        SimNumber        ="integer",
        Spp1Price        ="numeric",
        Spp2Price        ="numeric",
        EffortPrice      ="numeric",
        Increments       ="numeric",
        AddNoFishing     ="logical",
        Spp1DiscardSteps ="numeric",
        Spp2DiscardSteps ="numeric",
        ChoiceDist       ="integer"),
	prototype=prototype(
	      Spp1Uplimit      =as.double(8),
        Spp2Uplimit      =as.double(8),
        EffortUplimit    =as.integer(8),
        SimNumber        =as.integer(10),
        Spp1Price        =as.double(30),
        Spp2Price        =as.double(30),
        EffortPrice      =as.double(30),
        Increments       =as.double(30),
        AddNoFishing     =TRUE,
        Spp1DiscardSteps =as.double(0),
        Spp2DiscardSteps =as.double(0),
        ChoiceDist       =as.integer(0)),
	validity=validDynState.control

)

setClass("Sim",
	representation(
        Choice       = "array",
        Derivative   = "array",
        Spp1Rand     = "array",
        Spp2Rand     = "array",
        Spp1Landings = "array",
        Spp2Landings = "array",
        Spp1Hold     = "array",
        Spp2Hold     = "array",
        Spp1Discards = "array",
        Spp2Discards = "array",
        Effort       = "array",
        Exceed       = "array"),

	prototype=prototype(
        Choice       = array(),
        Derivative   = array(),
        Spp1Rand     = array(),
        Spp2Rand     = array(),
        Spp1Landings = array(),
        Spp2Landings = array(),
        Spp1Hold     = array(),
        Spp2Hold     = array(),
        Spp1Discards = array(),
        Spp2Discards = array(),
        Effort       = array(),
        Exceed       = array())
)

invisible(createFLAccesors(new("Sim")))

setClass("DynState",
	representation(
        Sim         ="Sim",
        control     ="DynState.control",
        ChoiceDist  ="array"),
	prototype=prototype(
        Sim = new("Sim"),
        control     = new("DynState.control"),
        ChoiceDist  = array() )
)

invisible(createFLAccesors(new("DynState")))

DynState.control <- function(Spp1Uplimit=4000, Spp2Uplimit=4000, EffortUplimit=5, SimNumber=30, Spp1Price=4, Spp2Price=3, EffortPrice=500, Increments=15, AddNoFishing=TRUE, Spp1DiscardSteps=5, Spp2DiscardSteps=5, ChoiceDist=0){
    res <- new("DynState.control", Spp1Uplimit= as.double(Spp1Uplimit), Spp2Uplimit=as.double(Spp2Uplimit), EffortUplimit=as.integer(EffortUplimit),
      SimNumber=as.integer(SimNumber), Spp1Price=as.double(Spp1Price), Spp2Price=as.double(Spp2Price), EffortPrice=as.double(EffortPrice), Increments=as.integer(Increments),
      AddNoFishing=AddNoFishing, Spp2DiscardSteps=as.double(Spp2DiscardSteps), Spp1DiscardSteps=as.double(Spp1DiscardSteps), ChoiceDist=as.integer(ChoiceDist))
    return(res)
}

DynState <- function(inputSpp1,inputSpp2,inputEffort, control){

  if (!inherits(inputSpp1,"DynStateInput"))   stop("inputSpp1 should be of class DynStateInput")
  if (!inherits(inputSpp2,"DynStateInput")) stop("inputSpp2 should be of class DynStateInput")
  if ((dim(inputSpp1@CatchMean)[6]>1)|(dim(inputSpp1@CatchSigma)[6]>1)|(dim(inputSpp2@CatchMean)[6]>1)|(dim(inputSpp2@CatchSigma)[6]>1)) stop("No 6th dimension allowed")
  if (dim(inputSpp1@CatchMean)[4]>12) stop("No more than 12 timesteps allowed")
  
  # ----------------------------- Calculate length of vector of steps for separate spec

  bin1size <- round(max(inputSpp1@CatchMean + (3*inputSpp1@CatchSigma)) / control@Increments, 3)
  bin2size <- round(max(inputSpp2@CatchMean + (3*inputSpp2@CatchSigma)) / control@Increments, 3)


  # ----------------------------- Check which vector is longest and use that number of steps

  k <- control@Increments
  NumInputAreas <- dim(inputSpp1@CatchMean)[5]
  MaxAreas <- NumInputAreas*((control@Spp1DiscardSteps+1)*(control@Spp2DiscardSteps+1))
  
  # ----------------------------- Cap uplimits to model size
  control@Spp1Uplimit <- min(control@Spp1Uplimit,  ((k*dim(inputSpp1@CatchMean)[4])-1)* bin1size )
  control@Spp2Uplimit <- min(control@Spp2Uplimit,  ((k*dim(inputSpp1@CatchMean)[4])-1)* bin2size )

  # ----------------------------- Dimensionalitychecks

  if (MaxAreas > 32760 ) stop("More than 32760 options, too many to fit in model settings") 
  if ( (control@Spp1Uplimit /bin1size) > ( k*dim(inputSpp1@CatchMean)[4])) stop("Spp1Uplimit set too high")
  if ( (control@Spp2Uplimit /bin2size) > ( k*dim(inputSpp2@CatchMean)[4])) stop("Spp2Uplimit set too high")

  # ---------------------------- Build vector of equal length for both species, starting at very large neg number so that sum always =1

  spp1inc <- seq(0,k*bin1size,bin1size)
  spp2inc <- seq(0,k*bin2size,bin2size)
  spp1inc[1] <- -8000
  spp2inc[1] <- -8000
  spp1inc[length(spp1inc)] <- 200000000
  spp2inc[length(spp2inc)] <- 200000000

  # ---------------------------- The structure of catchparms in the model is patch, season, species, increment

  catchparms <- array(NA,dim=c(NumInputAreas,(control@Spp1DiscardSteps+1), (control@Spp2DiscardSteps+1),dim(inputSpp1@CatchMean)[4],2,k))

  for (i in 1:NumInputAreas){ # the area
      for (j in 1:dim(inputSpp2@CatchMean)[4]){ # the season
            a <- pnorm(spp1inc,inputSpp1@CatchMean[,,,j,i],inputSpp1@CatchSigma[,,,j,i])
            b <- pnorm(spp2inc,inputSpp2@CatchMean[,,,j,i],inputSpp2@CatchSigma[,,,j,i])
            catchparms[i,1,,j,1,] <- rep(a[2:(k+1)] - a[1:k], each=control@Spp2DiscardSteps+1)
            catchparms[i,,1,j,2,] <- rep(b[2:(k+1)] - b[1:k], each=control@Spp1DiscardSteps+1)
  }}



   for (i in 1:NumInputAreas) {# the area
      for (j in 1:dim(inputSpp2@CatchMean)[4]){ # the season
              for (dp in 0:control@Spp1DiscardSteps){
                   if (dp >0 && dp != control@Spp1DiscardSteps) {
                       spltvec <- floor(k*(1- dp/(control@Spp1DiscardSteps)))
                       catchparms[i,dp+1,, j, 1,] <- rep(c(catchparms[i,1,1, j, 1,1:spltvec],sum(catchparms[i,1,1, j, 1,(spltvec+1):k]),rep(0,k-spltvec-1)), each=control@Spp2DiscardSteps+1)   #plaice
                   }
                   if (dp > 0 && dp == control@Spp1DiscardSteps) catchparms[i,dp+1,, j, 1,] <- rep(c(1,rep(0,k-1)), each=control@Spp2DiscardSteps+1)   #plaice
              }
              for (ds in 0:control@Spp2DiscardSteps) {
                  if (ds > 0 && ds != control@Spp2DiscardSteps) {
                     spltvec <- floor(k*(1- ds/(control@Spp2DiscardSteps)))
                     catchparms[i,,ds+1,j, 2,] <- rep(c(catchparms[i,1,1, j, 2,1:spltvec],sum(catchparms[i,1,1, j, 2,(spltvec+1):k]),rep(0,k-spltvec-1)), each=control@Spp1DiscardSteps+1)     #sole
                  }
                  if (ds > 0 &&  ds == control@Spp2DiscardSteps) catchparms[i,,ds+1, j, 2,] <- rep(c(1,rep(0,k-1)), each=control@Spp1DiscardSteps+1)
              }
  }}

  inputEffort <- rep(inputEffort,((control@Spp2DiscardSteps+1) * (control@Spp1DiscardSteps+1)))

  dim(catchparms) <- c(MaxAreas,dim(inputSpp2@CatchMean)[4],2,k) # collapse first three dims so we get each derivative area in first dim
  ctcparms <- array(NA,dim=c(MaxAreas + control@AddNoFishing,dim(inputSpp2@CatchMean)[4],2,k))
  ctcparms[1:MaxAreas,,,] <- catchparms

  

  if (slot(control, "AddNoFishing")){
      # add zero catchrates in highest patch which is not fishing
      for (j in 1:dim(inputSpp2@CatchMean)[4])  { # the season
          ctcparms[MaxAreas + 1,j,1, 1] <- 1
          ctcparms[MaxAreas + 1,j,1,-1] <- 0
          ctcparms[MaxAreas + 1,j,2, 1] <- 1
          ctcparms[MaxAreas + 1,j,2,-1] <- 0

      }
      inputEffort <- c(inputEffort,0)
  }



  # ---------------------------- Test if patchnumber in costarray is equal to patchnumber catchparms
  if (length(inputEffort) != dim(ctcparms)[1]) stop ("Effortarray contains different number patches than catcharray")

  # ---------------------------- Call c++ code
  res <- .Call("DynState",ctcparms,inputEffort,control, bin1size, bin2size)

  # ---------------------------- Calculate derivative choice ------------------------

  res@Sim@Derivative <-
   ifelse(res@Sim@Choice == (MaxAreas + 1), (MaxAreas + 1),ifelse((res@Sim@Choice %% NumInputAreas)==0,NumInputAreas,(res@Sim@Choice %% NumInputAreas)))

  # --------------------------- Calculate Discarding --------------------------------

  res@Sim@Spp1Discards <- res@Sim@Spp1Landings
  res@Sim@Spp2Discards <- res@Sim@Spp2Landings

  for (i in 1:control@SimNumber) # the sims
      for (j in 1:dim(inputSpp2@CatchMean)[4]){
          res@Sim@Spp1Discards[i,j] <- (sum(cumsum(ctcparms[res@Sim@Derivative[i,j],j,1,]) < res@Sim@Spp1Rand[i,j])*bin1size) - res@Sim@Spp1Landings[i,j]
          res@Sim@Spp2Discards[i,j] <- (sum(cumsum(ctcparms[res@Sim@Derivative[i,j],j,2,]) < res@Sim@Spp2Rand[i,j])*bin2size) - res@Sim@Spp2Landings[i,j]
      }

  dimnames(res@Sim@Choice)       <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Derivative)   <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Exceed)       <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp1Landings) <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp2Landings) <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp1Hold)     <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp2Hold)     <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp1Discards) <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))
  dimnames(res@Sim@Spp2Discards) <- list(simulation=(1:control@SimNumber),season=(dimnames(inputSpp1@CatchMean)$season))

  res@control <- control

  # --------------------------- Add catchparms to results if required --------------------------------

  if (control@ChoiceDist == 1) res@ChoiceDist <- ctcparms
  return(res)

}

