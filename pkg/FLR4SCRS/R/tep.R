#### Total Egg Production
# Fecundity = exp(a) * stock.wt^b * diameter^c
# Egg = Fecundity * no. of mature females
# Egg = Fecundity * stock.n * sex ratio * prop mature
# i.e. the egg data is the total no. eggs produced at age
# Not by weight.  Not by individual.  The actual total eggs.
setGeneric("tep", function(object, params, ...){
	standardGeneric("tep")})
setMethod("tep", signature(object="FLStock",params="FLPar"),
   function(object,params) {

       fec <-exp(params["a"])*stock.wt(object)^params["b"]*params["diameter"]^params["c"]
       pmat<-sexRatio*mat(object)

    ## divide by wt so that ssb() works
    return(fec*pmat/stock.wt(object))})


