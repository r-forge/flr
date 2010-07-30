setMethod('setPlusGroup', signature(x='FLStock', plusgroup='numeric'),
  function(x, plusgroup, na.rm=FALSE, keepPlusGroup=TRUE,update=TRUE){
     x     <-FLCore::setPlusGroup(x, plusgroup,na.rm=na.rm,keepPlusGroup,update)
     
     ctrl  <-fwdControl(data.frame(quantity="f",val=fbar(x)[,dimnames(m(ple4))$year[-1],drop=T],year=as.numeric(dimnames(m(ple4))$year[-1])))
     x     <-fwd(x,ctrl=ctrl, sr=list(model="mean",params=FLPar(1)),sr.residuals=rec(x))
     
     return(x)})

