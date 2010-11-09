setGeneric("pgAge", function(object, ...){
	standardGeneric("pgAge")})
setMethod("pgAge", signature("FLStock"),
   function(object,pgAge=NULL,nAges=100,keepPlusGroup=TRUE){

    trueAge   =ac(range(object,"max")-1)

    dmns      =dimnames(harvest(object))
    dmns$age  =range(object,"plusgroup")
    pgAges    =FLQuant(NA,dimnames=dmns)

    #if mean age in 1st plus group not supplied approximate it assuming equilibrium
    if (is.null(pgAge)){
      dmns$age  =range(object,"max"):nAges
      dmns$year =1
      z.          =FLQuant(rep(c(harvest(object)[ac(range(object,"max")),1]+m(object)[ac(range(object,"max")),1]),each=nAges),dimnames=dmns)

      cumZ        =FLQuant(exp(-apply(z.@.Data,2:6.,cumsum)),dimnames=dimnames(z.))
      if (keepPlusGroup)
        cumZ[dim(cumZ)[1]]=cumZ[dim(cumZ)[1]]*(-1.0/(exp(-z.[dim(z.)[1]])-1.0))
      pgAge       =apply(sweep(cumZ,1,as.numeric(dmns$age),"*"),2:6,sum)/apply(cumZ,2:6,sum)
      }

    pgAges[,1]=pgAge

    surv=survivors(object)

    pg        =range(object,"plusgroup")

    for (iYr in 2:dims(object)$year)
       pgAges[,iYr]=((pgAges[,iYr-1]+1)*surv[ac(pg),iYr-1] +
                      pg*surv[ac(pg-1),iYr-1])/
                    (surv[ac(pg),iYr-1]+surv[ac(pg-1),iYr-1])

    return(pgAges)})
    
    
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

       fec =exp(params["a"])*stock.wt(object)^params["b"]*params["diameter"]^params["c"]
       pmat=sexRatio*mat(object)

    ## divide by wt so that ssb() works
    return(fec*pmat/stock.wt(object))})


