# Here are the functions, classes and methods that people may still be using but will no longer be maintained.
# They are kept for legacy reasons but may be deleted soon.

#**************** FLAssess.retro ***************************************

## FLAssess.retro	{{{
validFLAssess.retro <- function(object){
	# If the list is empty, then it is OK
	if (length(object) == 0)
		return(TRUE)
	# Make sure the list contains only numeric items
	for (i in 1:length(object))
		if (!inherits(object[[i]], "numeric"))
			return("Items must be numeric objects!")
	# Everything is fine
	return(TRUE)
}

setClass("FLAssess.retro",
	representation(
		desc    ="character",
    ssb     ="FLQuants",
		recruits="FLQuants",
		harvest ="FLQuants"),
	prototype=prototype(
		desc    =character(0),
		ssb     =new('FLQuants'),
		recruits=new('FLQuants'),
		harvest =new('FLQuants')
    ),
	validity=validFLAssess.retro
)

setValidity("FLAssess.retro", validFLAssess.retro)
remove(validFLAssess.retro)	# }}}

# retro   {{{
if (!isGeneric("retro"))
	setGeneric("retro", function(stock, indices, control, retro, ...)
    	standardGeneric("retro"))

setMethod('retro', signature(stock='FLStock', indices='FLIndex', control='ANY', 
  retro='numeric'),
  function(stock, indices, control, retro=1, ...)
    retro(stock=stock, indices=FLIndices(one=indices), control=control, retro=retro, ...))

setMethod('retro', signature(stock='FLStock', indices='FLIndices', control='ANY', 
  retro='numeric'),
  function(stock, indices, control, retro=1, year.range="missing")
  {

cat("\n\nWARNING: the retro class has been deprecated and is no longer maintained.
	Users are encouraged to make cunning use of tapply instead.
	See the assessment tutorials on the web, or the retro man pages for more information\n\n")

    minyear <- dims(stock)$minyear
    maxyear <- dims(stock)$maxyear

    # check usable years range
    if(missing(year.range))
      year.range <- (maxyear-retro):maxyear
    if(min(year.range) < minyear || max(year.range) > maxyear)
      stop("Year range outside stock object range")

    # Run that retrospective!
    cat("I am very pleased to run this retrospective for you...\n")
  
    tempindices <- indices
    res <- new("FLStocks")
    counter <- 0
    for (i in year.range)
    {
      counter <- counter + 1
      tempstock <- trim(stock, year=minyear:i)
      for (j in 1:length(tempindices))
      {
        min.yr <- min(as.numeric(dimnames(indices[[j]]@index)$year))
        max.yr <- max(as.numeric(dimnames(indices[[j]]@index)$year))
        if (i < min.yr) stop("year.range is outside indices year range")
          tempindices[[j]] <- trim(indices[[j]],year=min.yr:(min(max.yr,i)))
      }
    assess <- assess(control, tempstock, tempindices)
    tempstock <- tempstock + assess
    tempstock@name <- paste(tempstock@name, " Retrospective analysis for ", i, sep="")
    res[[as.character(i)]] <- tempstock
    }
    res@desc   <-paste("Retrospective analysis from object", stock@desc)
    return(res)
  }
) # }}}
