##############################################################
#' Maximum Sustainable Yield.
#'
#' Calculates MSY given the model parameters, can be done for a biodyn class, or by specifying the model and parameters
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{object}, a string or factor that species the model
#'
#' @param \code{params}, an \code{FLPar} object with model parameters
#'
#' @return an \code{FLPar} object with value(s) of MSY
#' 
#' @seealso \code{\link{bmsy}}, \code{\link{fmsy}} and  \code{\link{refpts}}
#' 
#' @export
#' @docType methods
#' @rdname msy
#'
#' @examples \dontrun{ msy("logistic",FLPar(msy=100,k=500))}
#'   
setMethod('msy',   signature(object='character', params="FLPar"),   function(object=factor(object), params=params)         msyFn(   object, params))
setMethod('msy',   signature(object='factor',    params="FLPar"),   function(object=       object,  params=params)         msyFn(   object, params))
setMethod('msy',   signature(object='biodyn',    params="missing"), function(object)   msyFn(model(object), params(object)))


##############################################################
#' Fishing Mortality at Maximum Sustainable Yield.
#'
#' Calculates $F_{MSY}$ given the model parameters, can be done for a biodyn class, or by specifying the model and parameters
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{object}, a string or factor that species the model
#'
#' @param \code{params}, an \code{FLPar} object with model parameters
#'
#' @return an \code{FLPar} object with value(s) of $F_{MSY}$
#' 
#' @seealso \code{\link{msy}}, \code{\link{bmsy}} and  \code{\link{refpts}}
#' 
#' @export
#' @docType methods
#' @rdname fmsy
#'
#' @examples
#' fmsy("logistic",FLPar(msy=100,k=500))
#'   
setMethod('fmsy',  signature(object='biodyn',    params="missing"), function(object)                                       fmsyFn(model(object), params(object)))
setMethod('fmsy',  signature(object='character', params="FLPar"),   function(object=factor(object), params=params)         fmsyFn(object,params))
setMethod('fmsy',  signature(object='factor',    params="FLPar"),   function(object=       object,  params=params)         fmsyFn(object,params))
  

##############################################################
#' Biomass at Maximum Sustainable Yield.
#'
#' Calculates $B_{MSY}$ given the model parameters, can be done for a biodyn class, or by specifying the model and parameters
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{object}, a string or factor that species the model
#'
#' @param \code{params}, an \code{FLPar} object with model parameters
#'
#' @return an \code{FLPar} object with value(s) of $MSY$, $B_{MSY}$ and $B_{MSY}$
#' 
#' @seealso \code{\link{msy}}, \code{\link{fmsy}} and  \code{\link{bmsy}}
#' 
#' @export
#' @docType methods
#' @rdname bmsy
#'
#' @examples
#' refpts("logistic",FLPar(msy=100,k=500))
#'  
  setMethod('bmsy',  signature(object='character', params="FLPar"),   function(object=factor(object), params=params)         bmsyFn(  object, params))
  setMethod('bmsy',  signature(object='factor',    params="FLPar"),   function(object=       object,  params=params)         bmsyFn(  object, params))
  setMethod('bmsy',  signature(object='biodyn',    params="missing"), function(object)                                       bmsyFn(model(object), params(object)))


##############################################################
#' Maximum Sustainable Yield reference points
#'
#' Calculates $MSY$, $B_{MSY}$ and $F_{MSY}$ given the model parameters, can be done for a biodyn class, or by specifying the model and parameters
#'
#' @param  \code{object}, an object of class \code{biodyn} or
#'
#' @param  \code{object}, a string or factor that species the model
#'
#' @param \code{params}, an \code{FLPar} object with model parameters
#'
#' @return an \code{FLPar} object with value(s) of $F_{MSY}$
#' 
#' @seealso \code{\link{msy}}, \code{\link{bmsy}} and  \code{\link{refpts}}
#' 
#' @export
#' @docType methods
#' @rdname refpts
#'
#' @examples
#' refpts("logistic",FLPar(msy=100,k=500))
#'  
setMethod('refpts', signature(object='character', params="FLPar"),   function(object=factor(object), params=params)          refptsFn(object, params))
setMethod('refpts', signature(object='factor',    params="FLPar"),   function(object=       object,  params=params)          refptsFn(object, params))
setMethod('refpts', signature(object='biodyn',    params="missing"), function(object)  {  model=model(object)
                                                                                          par  =params(object)
                                                                                          refptsFn(model,par)})

setMethod('refptSE', signature(object='character', params="FLPar"),   function(object=factor(object), params=params)         refptsFn(object, params))
setMethod('refptSE', signature(object='factor',    params="FLPar"),   function(object=       object,  params=params)         refptsFn(object, params))
setMethod('refptSE', signature(object='biodyn',    params="missing"), function(object)  {  model=model(object)
                                                                                           par  =params(object)
                                                                                           refptsFn(model,par)})


# Fox
msyFox  <- function(params)
  params["r"]*(params["k"]*exp(-1))*(1-(log(params["k"])-1)/log(params["k"]))

# Schaefer
msySchaefer <- function(params)
  params["r"]*params["k"]/4
msyLogistic <- function(params){
  r=4*params["msy"]/params["k"]
  r*params["k"]/4}

# PellaT
msyPellaT <- function(params)
  params["r"]*params["k"]*(1/(1+params["p"]))^(1/params["p"]+1)
msyGenfit <- function(params)
  params["r"]*params["k"]*(1/(1+params["p"]))^(1/params["p"]+1)

# Shepherd
msyShepherd<-function(params) {
  aPrime<-params["r"]/params["m"] - 1
  Bmax  <-params["k"]*aPrime
  .bmsy <- 0 #bmsy("shepherd",param)
  
  aPrime*params["m"]*.bmsy*(1-.bmsy/Bmax)/(1+aPrime)^.5}

# Gulland
msyGulland  <- function(params)
  (params["r"]*params["k"]^2)/4

msyFletcher <- function(params)
  params["msy"]

# Fox
bmsyFox  <- function(params)
  params["k"]*exp(-1)
# Schaefer
bmsySchaefer <- function(params)
  params["k"]/2
bmsyLogistic <- function(params)
  params["k"]/2
# PellaT
bmsyPellaT <- function(params)
  params["k"]*(1/(1+params["p"]))^(1/params["p"])
bmsyGenfit <- function(params)
  params["k"]*(1/(1+params["p"]))^(1/params["p"])
# Shepherd
bmsyShepherd <- function(params) {
  aPrime <- params["r"]/params["m"] - 1
  Bmax  <- params["k"]*aPrime
  
  Bmax*((1+aPrime)^.5-1)/aPrime}

# Gulland
bmsyGulland <-function(params)
  params["k"]/2
# Fletcher
bmsyFletcher <- function(params)
  params["k"]*(1/(params["p"]+1)^(1/(params["p"])))

fmsyFn=function(object,params,...){
  
  object=tolower(object)
  fmsyPellaT  <-function(params) params["r"]*(1/(1+params["p"]))
  fmsyFox     <-function(params) params["r"]*(1-(log(params["k"])-1)/log(params["k"]))
  fmsySchaefer<-function(params) params["r"]/2
  fmsyShepherd<-function(params) msyShepherd(params)/bmsyShepherd(params)
  fmsyGulland <-function(params) params["r"]*params["k"]/2
  fmsyFletcher<-function(params) msyFletcher(params)/bmsyFletcher(params)
  fmsyLogistic<-function(params) {r=4*params["msy"]/params["k"]; r/2}
  
  res<-switch(as.character(object),
              
              fox     =fmsyFox(     params),
              schaefer=fmsySchaefer(params),
              gulland =fmsyGulland( params),
              fletcher=fmsyFletcher(params),
              pellat  =fmsyPellaT(  params),
              shepherd=fmsyShepherd(params),
              logistic=fmsyLogistic(params),
              genfit  =fmsyPellaT(params))
  
  dimnames(res)$params="fmsy"
  
  return(res)}

msyFn=function(object,params,...) {
  
  object=tolower(object)
  res<-switch(object,
              fox      = msyFox(params),
              schaefer = msySchaefer(params),
              gulland  = msyGulland(params),
              fletcher = msyFletcher(params),
              pellat   = msyPellaT(params),
              shepherd = msyShepherd(params),
              logistic = msyLogistic(params),
              genfit   = msyPellaT(params))
  
  dimnames(res)$params="msy"
  
  return(res)}

bmsyFn=function(object,params) {
  
  object=tolower(object)
  res<-switch(object,
              fox     =bmsyFox(params) ,
              schaefer=bmsySchaefer(params),
              gulland =bmsyGulland(params),
              fletcher=bmsyFletcher(params),
              pellat  =bmsyPellaT(params),
              shepherd=bmsyShepherd(params),
              logistic=bmsySchaefer(params),
              genfit  =bmsyPellaT(params))
  
  dimnames(res)$params="bmsy"
  
  return(res)}

##############################################################
#' Calculate Carrying Capacity
#'
#' Calculates $k$ given msy, r and K for a Pella-Tomlinson biomass dynamic model
#'
#' @param  \code{msy}, a guess for $MSY$
#'
#' @param  \code{r}, a guess for $r$ the population growth rate
#'
#' @param \code{p}, a guess for $p$ the shape parameter
#'
#' @return an \code{FLPar} object with an estimate for $k$
#' 
#' @seealso \code{\link{msy}} and \code{\link{bmsy}} 
#' 
#' @export
#' @rdname calcK
#'
#' @examples
#' calcK(5000,r=.6,p=1.0)
calcK <- function(msy,r=.6,p=1,params=FLPar(r=r,p=p)){
  res=msy/(params["r"]*(1/(1+params["p"]))^(1/params["p"]+1))
  
  dimnames(res)$params="k"
  
  return(res)} 

refptsFn=function(model,params){
  model=tolower(model)
  dmns<-dimnames(params)
  names(dmns)[1]<-"refpts"
  dmns[[1]]<-c("msy","fmsy","bmsy")
  res<-FLPar(NA,dimnames=dmns)
  
  obj=as.character(model)
  
  res[ "msy"]<- msyFn(model, params)
  res["fmsy"]<-fmsyFn(model, params)
  res["bmsy"]<-bmsyFn(model, params)
  
  return(res)}
