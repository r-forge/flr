
## some useful methods and functions that make life simpler and code shorter

if (!isGeneric("Z"))
        setGeneric("Z", function(stock, ...)
                standardGeneric("Z"))


# Z(stock=FLStock)  {{{
setMethod("Z", signature(stock="FLStock"),
       function(stock, ...){
         if(all(is.na(harvest(stock))))
            stop('harvest values have not yet been calculated for FLStock object')

         return(harvest(stock)+m(stock))
})
     

#
# This method can be replaced by ... do.call('trim', c(list(FLQuant1), dimnames(FLQuant2)))
#
#if (!isGeneric("select"))  
#        setGeneric("select", function(flquant, dimnames, ...)
#                standardGeneric("select"))
#
##select(FLQuant, dimnames)
#setMethod("select", signature(flquant="FLQuant", dimnames="list"),
#       function(flquant, dimnames, ...){
#
#   # check quants are the same in both arguments
#   qqname <- quant(flquant)
#   if(qqname != names(dimnames[1]))
#      stop('Inconsistent quant arguments, check quant of FLQuants')
#
#   # check that dimnames contains the correct names
#   if(!all(is.element(names(dimnames(flquant)), c(qqname, names(dimnames(FLQuant()))[2:6]))))
#      stop('dimnames names are incorrect')
#
#   return(trim(flquant, age=unlist(dimnames[1]), year=dimnames$year, 
#                        unit=dimnames$unit, season =dimnames$season, 
#                        area=dimnames$area, iter = dimnames$iter))
#
#})




# TODO: replace needs to match the method in base - function (x, list, values) 
#if(!isGeneric("replace"))
#        setGeneric("replace", function(quant1, quant2, ...)
#                standardGeneric("replace"))
#setMethod("replace", signature(quant1="FLQuant", quant2="FLQuant"),
#     function(quant1, quant2, ...) {
#
#  # check quants have the same dimensions
#  if(any(dim(quant1)!= dim(quant2)))
#    stop("FLQuant objects have different dimensions")
#
#  df1 <- as.data.frame(quant1)
#  df2 <- as.data.frame(quant2)
#
#  df1[is.na(df1$data),] <- df2[is.na(df1$data),]
#
#  return(as.FLQuant(df1))
#
#})



