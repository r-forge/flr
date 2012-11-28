setGeneric('readU',   function(file,method,...) standardGeneric('readU'))

setMethod('readU',  signature(file='character',method="missing"), function(file,method=method,...) {
  
  ln=tolower(scan(file,nlines=1,what=character(),sep="\n"))
  
  if (length(grep("multifan",ln))>0 | length(grep("mfcl",ln))>0)
    method="mfcl"
  if (length(grep("bot",ln))>0)
    method="aspic"
  
  switch(substr(method[1],1,2),
         ad=readU2box( file,...),
         as=readUaspic(file,...),
         bs=readUbsp(  file,...),
         mf=readUmfcl( file,...),
         ss=readUss(   file,...),
         vp=readUvpa(  file,...))
})

setMethod('readU',  signature(file='character',method="character"), function(file,method=c("aspic","adapt","bsp","mfcl","ss","sam","vpa"),...) {
      
    type=tolower(method)
    if (any("2box" == method)) method["2box" == method]="adapt"   
    switch(substr(method[1],1,2),
           ad=readU2box( file,...),
           as=readUaspic(file,...),
           bs=readUbsp(  file,...),
           mf=readUmfcl( file,...),
           ss=readUss(   file,...),
           vp=readUvpa(  file,...))
    })

setGeneric('readU2box',   function(x,...) standardGeneric('readU2box'))
setGeneric('readUvpa',    function(x,...) standardGeneric('readUvpa'))
setGeneric('readUss',     function(x,...) standardGeneric('readUss'))
setGeneric('readUmfcl',   function(x,...) standardGeneric('readUmfcl'))
setGeneric('readUbsp',    function(x,...) standardGeneric('readUbsp'))
setGeneric('readUaspic',  function(x,...) standardGeneric('readUaspic'))
 
setMethod('readU2box',  signature(x='character'), function(x,...) iU2box(    x,...))
setMethod('readUvpa',   signature(x='character'), function(x,...) iUVPASuite(x,...))
setMethod('readUss',    signature(x='character'), function(x,...) iUSS(      x,...))
setMethod('readUmfcl',  signature(x='character'), function(x,...) iUMFCL(    x,...))
setMethod('readUbsp',   signature(x='character'), function(x,...) iUBSP(     x,...))
setMethod('readUaspic', signature(x='character'), function(x,...) iUAspic(   x,...))
 
