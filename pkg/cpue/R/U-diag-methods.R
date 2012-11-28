setGeneric('diagU',   function(file,method,...) standardGeneric('diagU'))

setMethod('diagU',  signature(file='character',method="character"), function(file,method=c("aspic","adapt","bsp","mfcl","ss","sam"),...) {
  
  method=tolower(method)
  if (any("2box" == method)) method["2box" == method]="adapt"   
  switch(substr(method[1],1,2),
         ad=diagU2box( file,...),
         as=diagUaspic(file,...),
         bs=diagUbspc( file,...),
         mf=diagUmfcl( file,...),
         ss=diagUss(   file,...),
         sa=diagUsam(  file,...))
})


setGeneric('diagU2box',   function(x,...) standardGeneric('diagU2box'))
setGeneric('diagUsam',    function(x,...) standardGeneric('diagUsam'))
setGeneric('diagUss',     function(x,...) standardGeneric('diagUss'))
setGeneric('diagUmfcl',   function(x,...) standardGeneric('diagUmfcl'))
setGeneric('diagUbsp',    function(x,...) standardGeneric('diagUbsp'))
setGeneric('diagUaspic',  function(x,...) standardGeneric('diagUaspic'))


setMethod('diagU2box',  signature(x='character'), function(x,...) .diagU2box( x,...))
setMethod('diagUsam',   signature(x='character'), function(x,...) .diagUsam(  x,...))
setMethod('diagUss',    signature(x='character'), function(x,...) .diagUss(   x,...))
setMethod('diagUmfcl',  signature(x='character'), function(x,...) .diagUmfcl( x,...))
setMethod('diagUbsp',   signature(x='character'), function(x,...) .diagUbsp(  x,...))
setMethod('diagUaspic', signature(x='character'), function(x,...) .diagUaspic(x,...))


