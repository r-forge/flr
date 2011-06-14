#### Indicators ################################################################
setGeneric("mnLen", function(object,...)
   standardGeneric("mnLen"))
setGeneric("wt2z", function(object,...)
   standardGeneric("wt2z"))
setGeneric("ln2z", function(object,Linf,...)
    standardGeneric("ln2z"))

setMethod('mnLen', signature(object='FLStock'), 
    function(object,a=0.001,b=3,wt="stock.wt") 
	  mnLenFunc(object,a,b,wt))
setMethod('wt2z', signature(object='FLStock'), 
    function(object,a=0.001,b=3,wt="stock.wt") 
          wt2zFunc(object,a,b,wt))
setMethod('ln2z', signature(object='numeric',Linf='numeric'),
    function(object,Linf,Lc,k) 
	  ln2zFunc(object,Linf,Lc,k))
setMethod('ln2z', signature(object='numeric',Linf="FLPar"),
    function(object,Linf) 
	  ln2zFunc(object,Linf["Linf"],Linf["Lc"],Linf["k"]))

mnLenFunc<-function(object,a=0.001,b=3,wt="stock.wt"){
    wt.=slot(object,wt)
    n. =slot(object,paste(slot(object,wt)[[1]][1],",n"))

    apply((wt./a)^(1/b)*n.,c(2,6),sum)/apply(n.,c(2,6),sum)}

wt2zFunc<-function(object,Linf,Lc,k,a=0.001,b=3,wt="stock.wt"){
    mnSz<-mnSzStock(object,a,b,wt); 
    k*(Linf-mnSz)/(mnSz-Lc)}

ln2zFunc<-function(object,Linf,Lc,k){
    k*(Linf-object)/(object-Lc)}
################################################################################

