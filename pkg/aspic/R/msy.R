fletcherSP =function(biomass,params,p=2) {
      lambda =(p^p/(p-1))/(p-1)
      lambda*params["msy"]*(biomass/params["k"])-lambda*params["msy"]*(biomass/params["k"])^p}

fmsy=function(object) msyFletcher(object@params)/bmsyFletcher(object@params)
 msy=function(object) object@params["msy"]
bmsy=function(object,p=2) object@params["k"]*(1/p)^(1/(p-1))

 
   
