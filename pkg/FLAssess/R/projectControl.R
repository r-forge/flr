
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 06 Mar 2009 19:17
# $Id: fwdControl.R 232 2009-04-30 15:44:58Z fscott $

validProjectControl <- function(object){
	return(TRUE)

#  if (dim(object@target)[1]!=dim(object@trgtArray)[1]){
#     warning("rows in target & trgtArray don't match")
#     return(FALSE)}

#  if (any(object@target[,"quantity"] %in% names(quantity))){
#     warning("quantity not recognised")
#     return(FALSE)}
     
#  if (length(slot(object, 'effort'))>0){
#     if (dim(object@effort)[1]!=dim(object@effArray)[1]){
#        warning("rows in effort & effArray don't match")
#        return(FALSE)}
      
#     if (dim(object@target)[1]!=dim(object@effort)[1]){
#        warning("rows in target & effort don't match")
#        return(FALSE)}   
 
#     if (dim(object@trgtArray)[3]!=dim(object@effArray)[3]){
#        warning("iters in trgtArray & effArray don't match")
#        return(FALSE)}   
#     }
     
	# Everything is fine
#	return(TRUE)
}

setClass("projectControl",
	representation(
		target   ="data.frame",
                trgtArray="array"),
	prototype=prototype(
		target   =data.frame(NULL),
                trgtArray=array()),
	validity=validProjectControl
  )


           
if (!isGeneric("projectControl")) {
	setGeneric("projectControl", function(object, ...){
		value  <-  standardGeneric("projectControl")
		value
	})}  

setMethod("projectControl", signature(object='missing'),
     function(...){
        return(new('projectControl'))
     }
)

setMethod("projectControl", signature(object="data.frame"),
 projectControl.<-function(object,effort=NULL,trgtArray=NULL,effArray=NULL,...){

    ##### Internal Functions ###################################################
    trgtNms<-c("year","quantity","val")  
    effNms <-c("year","val")  

    df<-function(yrs,nms){
      df<-NULL
      for (i in nms)
         df<-cbind(df,rep(NA,length(yrs)))
      dimnames(df)<-list(1:length(yrs),nms)
      return(data.frame(df))
      }

   ##### End Internal Functions ###############################################

    if (!is(object,"data.frame"))
       stop("target not data.frame")

    if (!("year" %in% names(object)))
       stop("year not specified in object")
    yrs<-object[,"year"]

    res<-new("projectControl")
   
    ##Targets ##################################################################
    ## Create complete target data frame        
    res@target<-df(yrs,trgtNms)
    res@target[,dimnames(object)[[2]]]<-object[,dimnames(object)[[2]]]
#    if (!checkTarget(res@target))
#       stop("target not valid")

   return(res)
   })

showArray<-function(object){
    if(dim(object)[3] > 1){
		  v1 <- apply(object, 1:2, median, na.rm=TRUE)
  		v2 <- apply(object, 1:2, mad,    na.rm=TRUE)	 
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")}
    else
      v3 <- paste(format(apply(object, 1:2, median, na.rm=TRUE),digits=5))
		
    print(array(v3, dim=dim(object)[1:2], dimnames=dimnames(object)[1:2]), quote=FALSE)

		if(dim(object)[3] != 1)
			cat("iters: ", dim(object)[3],"\n\n")}

setMethod('show', signature(object='projectControl'), 
  function(object){
    
  cat("\nTarget\n")
  print(slot(object, 'target'))
  showArray(object@trgtArray)
     
  if (length(slot(object, 'effort'))>0){
     cat("\n\nEffort\n")
     print(slot(object, 'effort'))
     showArray(object@effArray)}
  })

chkFwdControl<-function(ctrl,sr,x,y=NULL){
   if (is(x,"FLStock")){
      
      return(ctrl)
      }
   else if (is(x,"FLBiol")){
      return(ctrl)
      }
      
   }
