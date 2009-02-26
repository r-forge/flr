# fwd.R
# FLash/R/fwd.R
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas
# Last Change: 27 Nov 2007 19:20
# $Id: fwdControl.R,v 1.14 2009/02/18 08:44:47 ltkell Exp $

validFwdControl <- function(object){
	return(TRUE)

  if (dim(object@target)[1]!=dim(object@trgtArray)[1]){
     warning("rows in target & trgtArray don't match")
     return(FALSE)}

  if (any(object@target[,"quantity"] %in% names(quantity))){
     warning("quantity not recognised")
     return(FALSE)}
     
  if (length(slot(object, 'effort'))>0){
     if (dim(object@effort)[1]!=dim(object@effArray)[1]){
        warning("rows in effort & effArray don't match")
        return(FALSE)}
      
     if (dim(object@target)[1]!=dim(object@effort)[1]){
        warning("rows in target & effort don't match")
        return(FALSE)}   
 
     if (dim(object@trgtArray)[3]!=dim(object@effArray)[3]){
        warning("iters in trgtArray & effArray don't match")
        return(FALSE)}   
     }
     
	# Everything is fine
	return(TRUE)
  }

setClass("fwdControl",
	representation(
		target   ="data.frame",
		effort   ="data.frame",
    trgtArray="array",
		effArray ="array"),
	prototype=prototype(
		target   =data.frame(NULL),
		effort   =data.frame(NULL),
    trgtArray=array(),
		effArray =array()),
	validity=validFwdControl
  )
           
if (!isGeneric("fwdControl")) {
	setGeneric("fwdControl", function(object, ...){
		value  <-  standardGeneric("fwdControl")
		value
	})}  

setMethod("fwdControl", signature(object="data.frame"),
fwdControl.<-function(object,effort=NULL,trgtArray=NULL,effArray=NULL,...){

    ##### Internal Functions ###################################################
    trgtNms<-c("year","quantity","min","val","max","spp","fleet","metier","rel")  
    effNms <-c("year","min","val","max","fleet","metier","rel.year","rel.fleet","rel.metier","rel.bound")  

    setArray<-function(x,nrws,nits=NULL,type="trgtArray"){   
       if (is(x,"list") & any(names(x) %in% c("min","val","max"))){
         if (!all(lapply(x,class) %in% c("array","matrix","numeric")))
            stop(paste(type,": elements of list neither 'array', 'matrix' or 'numeric'"))

         if (is.null(nits)) 
            if      (is(x[[1]],"numeric"))                     nits<-length(x[[1]])
            else if (is(x[[1]],"array") | is(x[[1]],"matrix")) nits<-dim(x[[1]])[length(dim(x[[1]]))]
            else stop("")

         res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iters=1:nits))
         if ("val" %in% names(x)){
            if (is.vector(x$val)) x$val<-array(x$val,dim=c(1,length(x$val)))
            if (nits == dim(x$val)[2])
               res[,"val",]<-x$val
            }
         if ("min" %in% names(x)){
            if (is.vector(x$min)) x$min<-array(x$min,dim=c(1,length(x$min)))
            if (nits == dim(x$min)[2])
               res[,"min",]<-x$min
            }
         if ("max" %in% names(x)) {
            if (is.vector(x$max)) x$max<-array(x$max,dim=c(1,length(x$max)))
            if (nits == dim(x$max)[2])
               res[,"max",]<-x$max}
            }
       else if (is(x,"array") & (length(dim(x))==3)){
          if (is.null(nits)) 
             nits<-dim(x)[3]
            
          res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iters=1:nits))     
    
          res[dimnames(x)[[1]],dimnames(x)[[2]],]<-x
          } 
       else stop("Has to be either a 3D array or list with 'min', 'max' or 'val' vectors")

       return(res)
       }

    df<-function(yrs,nms){
      df<-NULL
      for (i in nms)
         df<-cbind(df,rep(NA,length(yrs)))
        
      dimnames(df)<-list(1:length(yrs),nms)
    
      return(data.frame(df))
      }

    checkMinMax<-function(object)
        {
        # check that if max or min specified then no target & vice versa
        if (any((!is.na(object[,"min"]) | !is.na(object[,"max"])) & !is.na(object[,"val"]))) {
           cat("Can't specify val and both a min or max values")
           return(FALSE)}
        else if (any((!is.na(object[,"min"]) & !is.na(object[,"max"])) & object[,"max"]<=object[,"min"])){
           cat("max less than than min value")
           return(FALSE)}
        else
           return(TRUE)
        }
    ##### End Internal Functions ###############################################

    if (!is(object,"data.frame"))
       stop("target not data.frame")

    if (!("year" %in% names(object)))
       stop("year not specifierd in object")
    yrs<-object[,"year"]

    res<-new("fwdControl")
   
    ##Targets ##################################################################
    ## Create complete target data frame        
    res@target<-df(yrs,trgtNms)
    res@target[,dimnames(object)[[2]]]<-object[,dimnames(object)[[2]]]
    if (!checkTarget(res@target))
       stop("target not valid")

    if (!is.null(trgtArray)){ 
       res@trgtArray<-setArray(trgtArray,length(yrs),type="trgtArray")
       if (length(dim(res@trgtArray[,1,]))==2){
          res@target[,"min"]<-apply(res@trgtArray[,"min",],1,median)
          res@target[,"max"]<-apply(res@trgtArray[,"max",],1,median)
          res@target[,"val"]<-apply(res@trgtArray[,"val",],1,median)}
      else{
          res@target[,"min"]<-median(res@trgtArray[,"min",])
          res@target[,"max"]<-median(res@trgtArray[,"max",])
          res@target[,"val"]<-median(res@trgtArray[,"val",])}}
    else{
       res@trgtArray<-array(as.numeric(NA),dim=c(length(res@target[,1]),3,1),dimnames=list(1:length(res@target[,1]),c("min","val","max"),iters=1))}

    res@target[,"quantity"]<-factor(res@target[,"quantity"],levels=c("ssb","biomass","catch","landings","discards","f","z","f.landings","f.discards","effort","costs","revenue","profit","mnsz"))
    
    for (i in 1:length(res@target[,1])){
       if (any(is.na(res@trgtArray[i,"min",]))) res@trgtArray[i,"min",]<-res@target[i,"min"]
       if (any(is.na(res@trgtArray[i,"val",]))) res@trgtArray[i,"val",]<-res@target[i,"val"]
       if (any(is.na(res@trgtArray[i,"max",]))) res@trgtArray[i,"max",]<-res@target[i,"max"]}
     
    if (!checkMinMax(res@target)) { 
       cat(" in target\n")
       stop()}
       
    ##Effort ###################################################################
    if (!is.null(effort)){
      res@effort<-df(yrs,effNms)
      res@effort[ ,dimnames(effort)[[2]]]<-effort[,dimnames(effort)[[2]]]
      if (!is.null(effArray)) 
         res@effArray<-setArray(effArray,length(yrs),type="effArray")

      if (!is.null(effArray)){ 
         res@effArray<-setArray(effArray,length(yrs),type="effArray")
         if (length(dim(res@effArray[,1,]))==2){
            res@effort[,"min"]<-apply(res@effArray[,"min",],1,median)
            res@effort[,"max"]<-apply(res@effArray[,"max",],1,median)
            res@effort[,"val"]<-apply(res@effArray[,"val",],1,median)}
         else{
            res@effort[,"min"]<-median(res@effArray[,"min",])
            res@effort[,"max"]<-median(res@effArray[,"max",])
            res@effort[,"val"]<-median(res@effArray[,"val",])}}
      else
         res@effArray<-array(as.numeric(NA),dim=c(length(res@effort[,1]),3,1),dimnames=list(1:length(res@effort[,1]),c("min","val","max"),iters=1))
          
    for (i in 1:length(res@effort[,1])){
       if (any(is.na(res@effArray[i,"min",]))) res@effArray[i,"min",]<-res@effort[i,"min"]
       if (any(is.na(res@effArray[i,"val",]))) res@effArray[i,"val",]<-res@effort[i,"val"]
       if (any(is.na(res@effArray[i,"max",]))) res@effArray[i,"max",]<-res@effort[i,"max"]}
     
    if (!checkMinMax(res@effort)){ 
       cat(" in effort\n")
       stop()}}

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

setMethod('show', signature(object='fwdControl'), 
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