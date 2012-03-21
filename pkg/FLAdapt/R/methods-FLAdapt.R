setMethod('fit', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="vpa-2box", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))
setMethod('boot', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="vpa-2box", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))
setMethod('pro2box', signature(object='FLAdapt'),
  function(object, package="adapt", exeNm="pro-2box", dir=tempdir(),...) 
      runExe(object, package, exeNm, dir,...))

writeFn=function(object,exeNm="adapt") {
   return()}
  
readFn=function(object,exeNm="adapt") {

   return()}

setMethod('readVpa2box', signature(x='character'),
    function(x,what="FLStock",data.frame=TRUE,drop=TRUE,m=NULL,nRet=0,...){ 
      
      res=switch(tolower(what),
            "flstock"  =readVpa2boxFn(       x, args=list(...),m=m,nRet=nRet),
            "flindices"=readVpa2boxIndicesFn(x),
            "diags"    =readVpa2boxDiagsFn(  x))
      
      if (data.frame){
        if (tolower(what)=="flindices"){
           nms=names(res)
           res=ldply(idx, function(x) model.frame(x,drop=drop))
           res$Series=nms[res$i]}
           
        if (tolower(what)=="flstock"){
           if ("FLStocks" %in% is(res)){
             nms=names(res)
             res=ldply(idx, function(x) model.frame(x,drop=drop))
             res$Series=nms[res$i]}
           if ("FLStock" %in% is(res)){
             res= model.frame(x,drop=drop)
             res$Series=nms[res$i]}}}
 
      return(res)})


setGeneric('refpts<-', function(object, ..., value) standardGeneric('refpts<-'))

setMethod("refpts<-", signature(object="FLAdapt", value="character"),
  function(object, value) {
  	slot(object, "refpts") <- readPro2box(value,type="ref",data.frame=FALSE)[,1:4,]
		
  return(object)}) 

# setMethod("FLAdaptControl<-", signature(object="FLAdaptControl", value="character"),
#   function(object, value) {
#     slot(object, "FLAdaptControl") <- new("FLAdaptControl")
# 		
#   return(object)})

setMethod('refpts', signature(object='character'),
    function(object,data.frame=FALSE,file="BENCH-1.OUT",...) 
        createRefpts(object,data.frame=data.frame,file=file,...))

setMethod('readVpa2boxIndices', signature(x='character'),
    function(x,...) 
        readVpa2boxIndicesFn(x,...))

setMethod('readVpa2boxDiags', signature(x='character'),
    function(x,...) 
        readVpa2boxDiagsFn(x,...))


  