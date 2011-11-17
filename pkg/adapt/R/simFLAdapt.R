simFLAdapt=function(grwPar =FLPar(linf=122.8,t0=-0.9892,k=0.3013445,a=1.3718e-5,b=3.1066),
                    harvest=FLQuant(  c(seq(0,2.0,length.out=40),
                                 rev(seq(0.5,2.0,length.out=15))[-1],rep(0.5,10))),...){

  #args <- list(...)

  res      =gislaSim(grwPar)
  fbar(res)=harvest*refpts(res)["msy","harvest"]

  res=fwd(res)

  return(res)}
