setGeneric("cpue<-",    function(object,value,...) standardGeneric('cpue<-'))
#setGeneric("params<-",  function(object,value,...) standardGeneric('params<-'))
# 
setGeneric("bounds",    function(object,...)       standardGeneric('bounds'))
setGeneric("bounds<-",  function(object,value,...) standardGeneric('bounds<-'))
#setGeneric("catch<-",   function(object,value,...) standardGeneric('catch<-'))

setMethod('bounds',  signature(object='aspic'),
          function(object)  object@bounds)

setMethod('cpue<-',  signature(object='aspic',value="character"),
          function(object,value) {
              object@cpue=readU(value)
            
              return(object)
              })
#cpue(swon)="/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.inp"
           
setMethod('params<-',  signature(object='aspic',value="character"),
          function(object,value) {
            
            coerceDP=function(x)  FLPar(unlist(c(t(x))),params=names(x),iter=dim(x)[1])
            
            det=aspicDet(value)
            parNms=dimnames(object@params)$params
            object@params=coerceDP(det[,parNms])
            
            return(object)
          })
#params(swon)<-"/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.det"

setMethod('bounds<-',  signature(object='aspic',value="FLPar"),
          function(object,value,min=0.1,max=10.0,fix=T) {
            
            if (fix) nms=dimnames(value)$params[swon@bounds[,"fit"]==1] else
                     nms=dimnames(value)$params[swon@bounds[,"fit"]==1]
            
            object@bounds[nms,"start"]=value[nms]
            object@bounds[nms,"min"]  =value[nms]*min
            object@bounds[nms,"max"]  =value[nms]*max
            
            return(object)
          })
#bounds(swon)<-swon@params

setMethod('catch<-',  signature(object='aspic',value="character"),
          function(object,value) {
            object@cpue=readU(value)
            
            dat=ddply(object@cpue[object@cpue$code %in% c("CC","CE"),],.(year), with, data.frame(data=sum(catch)))
            
            object@catch=as.FLQuant(dat)

            return(object)})
#catch(swon)="/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.inp"




