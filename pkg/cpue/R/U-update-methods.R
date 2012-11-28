setGeneric('updateU',   function(x,from,method,...) standardGeneric('updateU'))

setMethod('updateU',  signature(x="data.frame",from='character',method="character"), function(x,from,method=c("aspic","adapt","bsp","mfcl","ss","vpa"),to=from,...) {
  
  method=tolower(method)
  if (any("2box" == method)) method["2box" == method]="adapt"   
  switch(substr(method[1],1,2),
         ad=updateU2box( x,from,to,,...),
         as=updateUaspic(x,from,to,...),
         bs=updateUbspc( x,from,to,...),
         mf=updateUmfcl( x,from,to,...),
         ss=updateUss(   x,from,to,...),
         vp=updateUvpa(  x,from,to,...))
})


setGeneric('updateU2box',   function(x,from,to,...) standardGeneric('updateU2box'))
setGeneric('updateUvpa',    function(x,from,to,...) standardGeneric('updateUvpa'))
setGeneric('updateUss',     function(x,from,to,...) standardGeneric('updateUss'))
setGeneric('updateUmfcl',   function(x,from,to,...) standardGeneric('updateUmfcl'))
setGeneric('updateUbsp',    function(x,from,to,...) standardGeneric('updateUbsp'))
setGeneric('updateUaspic',  function(x,from,to,...) standardGeneric('updateUaspic'))

setMethod('updateU2box',  signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateU2box(    x,from,to,...))
setMethod('updateUvpa',   signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateUVPASuite(x,from,to,...))
setMethod('updateUss',    signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateUSS(      x,from,to,...))
setMethod('updateUmfcl',  signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateUMFCL(    x,from,to,...))
setMethod('updateUbsp',   signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateUBSP(     x,from,to,...))
setMethod('updateUaspic', signature(x="data.frame",from='character',to="character"), function(x,from,to,...) .updateUAspic(   x,from,to,...))

