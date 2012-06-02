fletcherSP =function(biomass,params,p=2) {
      lambda =(p^p/(p-1))/(p-1)
      lambda*params["msy"]*(biomass/params["k"])-lambda*params["msy"]*(biomass/params["k"])^p}

fmsy=function(object) msyFletcher(object@params)/bmsyFletcher(object@params)
 msy=function(object) object@params["msy"]
bmsy=function(object,p=2) object@params["k"]*(1/p)^(1/(p-1))

smry=function(x){
  ##bugs
  maxyr = ac(dims(catch(x))$maxyear)
  r     = FLQuant(4*params(x)["msy"]/params(x)["k"])
  r.=r/r
  msy   = r.*FLQuant(params(x)["msy"])
  bmsy  = r.*FLQuant(params(x)["k"]/2)
  fmsy  = r.*2*r
  y.msy = r.*catch(x)[,maxyr]/(params(x)["k"]/2)
  b.bmsy= r.*stock(x)[,maxyr]/(params(x)["k"]/2)
  f.fmsy= r.*(catch(x)[,maxyr]/stock(x)[,maxyr])/fmsy
  sp    = r.*catch(x)[,maxyr]+stock(x)[,maxyr]-stock(x)[,ac(as.numeric(maxyr)+1)]
  sp.r  = r.*sp/r
  
  ##bug in that dimsnames do not change 
  #dimnames(r)=dimnames(b.bmsy)
  
  ## bug
  #as.data.frame(FLQuants("r"=r,"msy"=msy,"bmsy"=bmsy,"fmsy"=fmsy,"b.bmsy"=b.bmsy,"f.fmsy"=f.fmsy),drop=T)
  model.frame(FLQuants("r"=r,"msy"=msy,"bmsy"=bmsy,"fmsy"=fmsy,"b.bmsy"=b.bmsy,"f.fmsy"=f.fmsy,"sp"=sp,"sp.r"=sp.r),drop=T)
  }

