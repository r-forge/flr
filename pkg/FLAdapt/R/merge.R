# merge results from FLAdapt into FLStock
if (!isGeneric("merge")) {
    setGeneric("merge", useAsDefault = merge)}

setMethod("merge", signature(x="FLStock", y="FLAdapt"),
  function(x, y, ...){
    quant <- quant(stock.n(x))
    dnx <- dimnames(stock.n(x))
    dny <- dimnames(y@stock.n)

    # check dimensions match
    if(!all.equal(dnx[c(-2,-6)], dny[c(-2,-6)]))
      stop("Mismatch in dimensions: only year can differ between stock and assess")

    # same plusgroup
    if(x@range['plusgroup'] != x@range['plusgroup'])
      stop("Mismatch in plusgroup: x and y differ")

    # year ranges match? If not, trim the longest to equal the shortest
    if(!identical(dny[['year']],dnx[['year']]))
    {
      #Get common range
      common.rng <- range(as.numeric(intersect(dnx$year,dny$year)))
      x <- window(x,start=common.rng[1],end=common.rng[2])
      x@stock.n <- window(y@stock.n,start=common.rng[1],end=common.rng[2])
      x@harvest <- window(y@harvest,start=common.rng[1],end=common.rng[2])
    } else {
       x@stock.n <- y@stock.n
       x@harvest <- y@harvest
    } 
    x@desc <- paste(x@desc, "+ FLAdapt:", y@name)
    x@harvest@units <- y@harvest@units 
    x@range=c(unlist(dims(x)[c('min', 'max', 'plusgroup','minyear', 'maxyear')]),
      x@range[c('minfbar', 'maxfbar')])
        
    return(x)})

setMethod("+", signature(e1="FLStock", e2="FLAdapt"),
  function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")})

setMethod("+", signature(e1="FLAdapt", e2="FLStock"),
  function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e2, e1))
    else
      stop("Input objects are not valid: validObject == FALSE")})

setMethod("+", signature(e1="FLStock", e2="FLAdapts"),
  function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(FLStocks(lapply(e2,merge,x=e1)))
    else
      stop("Input objects are not valid: validObject == FALSE")})

setMethod("+", signature(e1="FLAdapts", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(FLStocks(lapply(e1,merge,x=e2)))
    else
      stop("Input objects are not valid: validObject == FALSE")})