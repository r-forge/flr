
validFLAdmb <- function(object){ 
  ## turn off 3:5 dims
  if (!all(dim(object@obs)[3:5] != dim(object@n)[3:5]) & !all(dim(object@n)[3:5]==1))
    warning("Only works for unique area, season and unit")
   
  return(TRUE)}

setClass('FLAdmb',
  representation(
    "FLComp",
    params        ='FLPar',
    bounds        ='array',
    priors        ='array',
 
    se            ='FLPar',
    vcov          ='FLPar',
    hessian       ='FLPar',
 
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  validity=validFLAdmb)
