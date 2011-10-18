simFLsz=function(grwPar =FLPar(linf=122.8,t0=-0.9892,k=0.3013445,a=1.3718e-5,b=3.1066),
                 harvest=FLQuant(  c(seq(0,2.0,length.out=40),
                                 rev(seq(0.5,2.0,length.out=15))[-1],rep(0.5,10))),...){

  args <- list(...)

  res      =gislaSim(grwPar,...)
  fbar(res)=harvest*refpts(res)["msy","harvest"]

  res=brp(res)

  # Load given slots
  for(i in names(args))
		slot(res, i) <- args[[i]]
  
  ctrl=fwdControl(data.frame(quantity="f",year=as.numeric(ac(dimnames(harvest)$year[-1])),quantity=c(harvest)[-1]))
  om  =fwd(as(res,"FLStock"),ctrl=ctrl,sr=res)

  grw=FLPar("linf"=c(grwPar["linf"]),"k"=c(grwPar["k"]),"lc"=c(grwPar["linf"])*0.05,"a"=c(grwPar["a"]),"b"=c(grwPar["b"]))
  
  zA=FLsz(om,grw=grw)
  
  return(list(om=om,zA=zA))}