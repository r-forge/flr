#### Convergence ###############################################################
# 1 100000     					## 0=no MC search, 1=search, 2=repeated srch; N trials #
# 1.00000e-08 					## Convergence crit. for simplex                       #  
# 3.00000e-08 6					## Convergence crit. for restarts, N restarts          #
# 1.00000e-04 0 				## Convergence crit. for estimating effort; N steps/yr #
# 8.00000 							## Maximum F allowed in estimating effort              #
################################################################################

model=factor(c("LOGISTIC", #Schaefer
               "GENGRID",  #generalized model at grid of values or at one specified value
               "FOX",      #Fox
               "GENFIT"))  #Fit the generalized model and estimate its exponent directly.

conditioning=c("YLD", #Condition fitting on yield (recommended for most analyses).
               "EFT") #Condition fitting on fishing-effort rate

objFn=c("SSE",     #Sum of squared errors (recommended default).
        "WTDSSE",  #SSE with annual data weighting
        "LAV")     #Least absolute values (robust objective function).

cpueCode=c("CE", "Fishing effort rate, catch (weight)",                  "Effort rate: annual average,Catch: annual total",
           "CC", "CPUE (weight-based), catch (weight)",                  "CPUE: annual average,Catch: annual total",
           "B0", "Estimate of biomass Effort rate: annual average",      "Start of year",
           "B1", "Estimate of biomass Catch: annual total",              "Annual average",
           "B2", "Estimate of biomass CPUE: annual average",             "End of year",
           "I0", "Index of biomass Catch: annual total",                 "Start of year",
           "I1", "Index of biomass Start of year",                       "Annual average", 
           "I2", "Index of biomass Annual average",                      "End of year")

cpueCode=t(array(cpueCode, dim=c(3,8),dimnames=list(c("code","desc","timing"),NULL)))
dimnames(cpueCode)[[1]]=cpueCode[,1]
cpueCode=transform(cpueCode[,-1],startf=c(0,0,0,0,1,0,0,1),
                                 endf  =c(1,1,0,1,1,0,1,1),
                                 ncol  =c(3,3,2,2,2,2,2,2),
                                 col2  =c("effort","index","biomass","biomass","biomass","index","index","index"),
                                 col3  =c("catch", "catch","",       "",       "",       "",     "",     ""))


validAspic <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")
  
  #model         ="factor"
  if (!("factor" %in% is(object@model)) || !(model %in% model))
    stop()
  
  #obj           ="factor",
  #conditioning  ="factor",
  #options       ="numeric",

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}

setClass('aspic', representation(
    "FLComp",
    model         ="factor",
    obj           ="factor",
    conditioning  ="factor",
    options       ="numeric",
    
    catch         ='FLQuant',
    stock         ='FLQuant',
    
    diags         ="data.frame",
    
    params        ='FLPar',
    bounds        ='array',
    stopmess      ="character",
    objFn         ="numeric",
    rnd           ="numeric"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA),   maxyear=as.numeric(NA))),
    model         =factor("LOGISTIC",levels=model,       labels=model),
    obj           =factor("SSE",     levels=objFn,       labels=objFn),
    conditioning  =factor("YLD",     levels=conditioning,labels=conditioning),
    options       =c(search=1,trials=100000,simplex=1e-8,restarts=3e-8,nrestarts=6,effort=1e-4,nsteps=0,maxf=8.0),
   
    catch         =FLQuant(),
    stock         =FLQuant(),

    params        =FLPar(NA,dimnames=list(param=c("b0","msy","k"),iter=1)),
    bounds        =array(NA,c(length(c(c("b0","msy","k"),paste("q",seq(1),sep=""))),5),dimnames=list(params=c(c("b0","msy","k"),paste("q",seq(1),sep="")),c("fit","min","start","max","lambda"))),
    vcov          =FLPar(NA,dimnames=list(param=c("b0","msy","k"),param=c("b0","msy","k"),iter=1)),
    hessian       =FLPar(NA,dimnames=list(param=c("b0","msy","k"),param=c("b0","msy","k"),iter=1)),
    stopmess      ="not ran"),
  validity=validAspic)
