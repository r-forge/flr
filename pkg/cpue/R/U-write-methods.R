setGeneric('writeU',   function(x,file,method,...) standardGeneric('writeU'))

setMethod('writeU',  signature(x="data.frame",file='character',method="character"), function(x,file,method=c("aspic","adapt","bsp","mfcl","ss","vpa"),...) {
  
  method=tolower(method)
  if (any("2box" == method)) method["2box" == method]="adapt"   
  switch(substr(method[1],1,2),
         ad=writeU2box( x,file,...),
         as=writeUaspic(x,file,...),
         bs=writeUbspc( x,file,...),
         mf=writeUmfcl( x,file,...),
         ss=writeUss(   x,file,...),
         vp=writeUvpa(  x,file,...))
})


setGeneric('writeU2box',   function(x,file,...) standardGeneric('writeU2box'))
setGeneric('writeUvpa',    function(x,file,...) standardGeneric('writeUvpa'))
setGeneric('writeUss',     function(x,file,...) standardGeneric('writeUss'))
setGeneric('writeUmfcl',   function(x,file,...) standardGeneric('writeUmfcl'))
setGeneric('writeUbsp',    function(x,file,...) standardGeneric('writeUbsp'))
setGeneric('writeUaspic',  function(x,file,...) standardGeneric('writeUaspic'))


setMethod('writeU2box',  signature(x="data.frame",file='character'), function(x,file,...) .writeU2box(    x,file,...))
setMethod('writeUvpa',   signature(x="data.frame",file='character'), function(x,file,...) .writeUVPASuite(x,file,...))
setMethod('writeUss',    signature(x="data.frame",file='character'), function(x,file,...) .writeUSS(      x,file,...))
setMethod('writeUmfcl',  signature(x="data.frame",file='character'), function(x,file,...) .writeUMFCL(    x,file,...))
setMethod('writeUbsp',   signature(x="data.frame",file='character'), function(x,file,...) .writeUBSP(     x,file,...))
setMethod('writeUaspic', signature(x="data.frame",file='character'), function(x,file,...) .writeUAspic(   x,file,...))
