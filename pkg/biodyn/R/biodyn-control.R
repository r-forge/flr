##############################################################
#' setControl
#'
#' Sets up the control slot in a biodyn object given the values in the \code{params}
#' slot. The starting values are set to the values in \code{params} and the min and
#' max bounds to .1 and 10 times of these.
#'
#' @param  \code{object}, an object of class \code{biodyn}
#'
#' @export
#' @docType methods
#' @rdname setControl
#'
#' @examples
#' /dintrun{
#' data(bd)
#' setControl(bd) <-params(bd)
#' params(bd)
#' control(bd)}
#' 
#'      
setGeneric('setControl<-', function(object,value,...)  standardGeneric('setControl<-'))

setMethod('setControl<-', signature(object='biodyn',value="FLPar"), function(object,value,min=0.1,max=10.0) {
  
    nms=dimnames(object@params)$params
    object@control=FLPar(array(rep(c(1,NA,NA,NA),each=length(nms)), dim=c(length(nms),4,1), dimnames=list(params=nms,option=c("phase","min","val","max"),iter=1)))
    object@control[nms,"val"]=value
    object@control[nms,"min"]=value[nms,]*min
    object@control[nms,"max"]=value[nms,]*max
    
    prr=object@priors
    object@priors=array(rep(c(0,0,0.3,1),each=length(nms)), dim=c(length(nms),4),   dimnames=list(params=nms,c("weight","a","b","type")))
    nms=dimnames(prr)$params[dimnames(prr)$params %in%  dimnames(object@priors)$params]
    
    object@priors[nms,]=prr[nms,]
    
    return(object)})

##############################################################
#' setParams
#'
#' Sets up the param slot in a biodyn object given an index
#'
#' @param  \code{object}, an object of class \code{biodyn}
#'
#' @export
#' @docType methods
#' @rdname setParams
#'
#' @examples
#' \dontrun{
#' data(bd)
#' setParams(bd) <-swonIndex
#' params(bd)}
#' 
setGeneric('setParams<-', function(object,value,...)  standardGeneric('setParams<-'))

setMethod('setParams<-', signature(object='biodyn',value="data.frame"), function(object,value) {
  
  nms=c(biodyn:::modelParams(as.character(object@model)),"b0")
  object@params=object@params[nms]
  
  object@params=setQ(object,value)
  
  return(object)})

setMethod('setParams<-', signature(object='biodyn',value="FLQuant"), function(object,value) {
  
  nms=c(biodyn:::modelParams(as.character(object@model)),"b0")
  object@params=object@params[nms]
 
  object@params=setQ(object,value)
  
  return(object)})

setMethod('setParams<-', signature(object='biodyn',value="FLQuants"), function(object,value) {
  
  nms=c(biodyn:::modelParams(as.character(object@model)),"b0")
  object@params=object@params[nms]
  
  object@params=setQ(object,value)
  
  return(object)})
