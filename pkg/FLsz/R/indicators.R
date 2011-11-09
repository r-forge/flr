#### Indicators ################################################################
##### Functions #############################################################################
setGeneric("ln2z",  function(object,param,...) standardGeneric("ln2z"))
setGeneric("mnwt",  function(object,...)       standardGeneric("mnwt"))
setGeneric("mnSwt", function(object,...)       standardGeneric("mnSwt"))
setGeneric("mnCwt", function(object,...)       standardGeneric("mnCwt"))
setGeneric("mnDwt", function(object,...)       standardGeneric("mnDwt"))
setGeneric("mnLwt", function(object,...)       standardGeneric("mnLwt"))

setGeneric("mnSln", function(object,param,...) standardGeneric("mnSln"))
setGeneric("mnCln", function(object,param,...) standardGeneric("mnCln"))
setGeneric("mnDln", function(object,param,...) standardGeneric("mnDln"))
setGeneric("mnLln", function(object,param,...) standardGeneric("mnLln"))

ln2zFunc<-function(object,linf,lc,k) k*(linf-object)/(object-lc)
setMethod('ln2z', signature(object='numeric',param="FLPar"),
    function(object,param) {
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln2zFunc(object,param["linf"],param["lc"],param["k"])})
setMethod('ln2z', signature(object='FLQuant',param="FLPar"),
    function(object,param) {
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln2zFunc(object,param["linf"],param["lc"],param["k"])})
setMethod('ln2z', signature(object='FLsz'),
    function(object) {
    param=object@grw
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln2zFunc(object@obs,param["linf"],param["lc"],param["k"])})

setMethod('mnSwt', signature(object='FLStock'), function(object) apply(stock.wt(   object)*stock.n(   object),2:6,sum)/apply(stock.n(   object),2:6,sum)) 
setMethod('mnCwt', signature(object='FLStock'), function(object) apply(catch.wt(   object)*catch.n(   object),2:6,sum)/apply(catch.n(   object),2:6,sum)) 
setMethod('mnLwt', signature(object='FLStock'), function(object) apply(landings.wt(object)*landings.n(object),2:6,sum)/apply(landings.n(object),2:6,sum)) 
setMethod('mnDwt', signature(object='FLStock'), function(object) apply(landings.wt(object)*discards.n(object),2:6,sum)/apply(discards.n(object),2:6,sum)) 

setMethod('mnSln', signature(object='FLStock',param="FLPar"), function(object,param) mnLenFunc(object,param,what="stock"))
setMethod('mnCln', signature(object='FLStock',param="FLPar"), function(object,param) mnLenFunc(object,param,what="catch"))
setMethod('mnDln', signature(object='FLStock',param="FLPar"), function(object,param) mnLenFunc(object,param,what="discards"))
setMethod('mnLln', signature(object='FLStock',param="FLPar"), function(object,param) mnLenFunc(object,param,what="landings"))

mnLenFunc<-function(object,param,what="stock"){
    dimnames(param)$params=tolower(dimnames(param)$params)  
    ln=(slot(object,paste(what,".wt",sep=""))/param["a"])^(1/param["b"])
    n =slot(object,paste(what,".n",sep=""))

    FLQuant(apply(ln*n,c(2,6),sum)/apply(n,c(2,6),sum),dimnames=dimnames(stock(object)))}
################################################################################

