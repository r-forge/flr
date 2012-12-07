bdModel=factor(c("pellat","fox","schaefer","gulland","fletcher","shepherd"))

validFLBioDym <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

#  if (!any(dimnames(index(object))$year %in% dimnames(catch(object))$year))
#      return("index has to cover some years of catches")
  
  #if(range$minyear < dims$minyear | range$maxyear > dims$maxyear)
  #  return("mismatch between range and object dimensions")

  return(TRUE)}

setClass('FLBioDym', representation(
    "FLComp",
    model         ="factor",
    distribution  ="factor",
    catch         ='FLQuant',
    index         ='FLQuant',
    stock         ='FLQuant',
    fitted        ='FLQuant',
    bounds        ='array',
    priors        ='array',
    params        ='FLPar',
    vcov          ='array',
    hessian       ='array',
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stopmess      ="character"),
  prototype(
    range       =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    catch       =FLQuant(),
    index       =FLQuant(),
    fitted      =FLQuant(),
    stock       =FLQuant(),
    model       =bdModel[3],
    distribution=factor("lnorm", levels=c("norm", "lnorm")),
    params      =FLPar(c(.5,NA,2,1,NA,NA),                    dimnames=list(param=c("r","k","p","b0","q","sigma"),iter=1)),
    bounds      =array(rep(c(1,NA,NA,NA),each=6), dim=c(6,4), dimnames=list(param=c("r","k","p","b0","q","sigma"),c("phase","lower","upper","start"))),
    priors      =array(rep(c(-1,0,0.3,1),each=6), dim=c(6,4), dimnames=list(param=c("r","k","p","b0","q","sigma"),c("weight","a","b","type")))
    ),
	validity=validFLBioDym) # }}}
