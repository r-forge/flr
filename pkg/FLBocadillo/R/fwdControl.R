#c.<-fwdControl(data.frame(year=1990:2000))
#c.@trgtArray

gtAry<-function(tA,x,val="val"){
   if (!is.null(x)){
     if (is.list(x))
        for (i in names(x))
           tA[i,val,]<-x[[i]]}

   return(tA)
   }

#gtAry(c.@trgtArray,list("1"=rep(1,10),"3"=3),"min")
#
#
#setTrgtArray<-function(x,nits=NULL,max=NULL,min=NULL,val=NULL){
#    res<-array(NA,lapply(dmns,length),dmns)
#
#    lapply(x,nits)
#
#
#    return(res)
#    }
#
#    setArray<-function(x,nrws,nits=NULL,type="trgtArray"){
#       if (is(x,"list") & any(names(x) %in% c("min","val","max"))){
#         if (!all(lapply(x,class) %in% c("array","matrix","numeric")))
#            stop(paste(type,": elements of list neither 'array', 'matrix' or 'numeric'"))
#
#         if (is.null(nits))
#            if      (is(x[[1]],"numeric"))                     nits<-length(x[[1]])
#            else if (is(x[[1]],"array") | is(x[[1]],"matrix")) nits<-dim(x[[1]])[length(dim(x[[1]]))]
#            else stop("")
#
#         res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iters=1:nits))
#         if ("val" %in% names(x)){
#            if (is.vector(x$val)) x$val<-array(x$val,dim=c(1,length(x$val)))
#            if (nits == dim(x$val)[2])
#               res[,"val",]<-x$val
#            }
#         if ("min" %in% names(x)){
#            if (is.vector(x$min)) x$min<-array(x$min,dim=c(1,length(x$min)))
#            if (nits == dim(x$min)[2])
#               res[,"min",]<-x$min
#            }
#         if ("max" %in% names(x)) {
#            if (is.vector(x$max)) x$max<-array(x$max,dim=c(1,length(x$max)))
#            if (nits == dim(x$max)[2])
#               res[,"max",]<-x$max}
#            }
#       else if (is(x,"array") & (length(dim(x))==3)){
#          if (is.null(nits))
#             nits<-dim(x)[3]
#
#          res<-array(NA,dim=c(nrws,3,nits),dimnames=list(1:nrws,c("min","val","max"),iters=1:nits))
#
#          res[dimnames(x)[[1]],dimnames(x)[[2]],]<-x
#          }
#       else stop("Has to be either a 3D array or list with 'min', 'max' or 'val' vectors")
#
#       return(res)
#       }
#
#if (!is.null(trgtArray)){
#   res@trgtArray<-setArray(trgtArray,length(yrs),type="trgtArray")
#   if (length(dim(res@trgtArray[,1,]))==2){
#      res@target[,"min"]<-apply(res@trgtArray[,"min",],1,median)
#      res@target[,"max"]<-apply(res@trgtArray[,"max",],1,median)
#      res@target[,"val"]<-apply(res@trgtArray[,"val",],1,median)}
#  else{
#      res@target[,"min"]<-median(res@trgtArray[,"min",])
#      res@target[,"max"]<-median(res@trgtArray[,"max",])
#      res@target[,"val"]<-median(res@trgtArray[,"val",])}}
#else{
#   res@trgtArray<-array(as.numeric(NA),dim=c(length(res@target[,1]),3,1),dimnames=list(1:length(res@target[,1]),c("min","val","max"),iters=1))}
