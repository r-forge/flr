setGeneric('simAR', function(object, ...)
  standardGeneric('simAR'))

setMethod('simAR', signature(object='FLSR'),
  function(object,start,end,n,nits){
  Recruitment=c(residuals(object))
  arobject  =acf(Recruitment,lag.max=n)
  plot(arobject)
  
  cvobject  =sd(residuals(object))
  objectRsdl=window(residuals(object),end=end)
  objectRsdl[]=0
  objectRsdl=log(rlnorm(nits,objectRsdl,cvobject))
  
  for( i in end:(dims(objectRsdl)$minyear+n))
    objectRsdl[,ac(i)]=apply(sweep(objectRsdl[,rev(ac(i-1:n))],2,arobject$acf[n:1],"*"),6,sum)
 
  return(objectRsdl)})

