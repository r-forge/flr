#bdModel=attributes(model(new("FLBioDym")))$levels
  # 
  # setMethod('aspic', signature(object="data.frame"),
  # asAspic=function(object,...){
  #   
  #             args <- list(...)
  #             
  #             res=new("aspic")
  #             
  #             ## The same
  #             slot(res,"desc")    =slot(object,"desc")
  #             slot(res,"name")    =slot(object,"name")
  #             slot(res,"range")   =slot(object,"range")
  #             
  #             slot(res,"catch")   =slot(object,"catch")
  #             slot(res,"stock")   =slot(object,"stock")
  #             
  #             slot(res,"stopmess")=slot(object,"stopmess")
  #       
  #             ## model
  #             slot(res,"model")=switch(model(object),
  #                                      schaefer=factor("LOGISTIC"),
  #                                      fox     =factor("FOX"),
  #                                      pellat  =factor("GENFIT"))
  #           
  #             slot(res,"params")  =paramFn(object)
  #             slot(res,"bounds")  =boundFn(res)
  #  
  #             # Load given slots
  #             for(i in names(args))
  #               slot(res, i) <- args[[i]]
  #                         
  #             return(res)}

paramFn=function(object){
  
#   fox       =c("r","K")
#   schaefer  =c("r","K")
#   pellat    =c("r","K","p")
  
  b2aParams=function(model,params) {
    
    model=model(bd)
    
    if(!(model %in% bdModel)) stop("has to be one of", bdModel)
    
    schaeferFn = function(biomass, params) { #logistic
      params["r"]=params["r"]*params["K"]/4
      dimnames(params)$params[1]="msy"
      
      params}
    
    foxFn =function(biomass, params) params
    
    pellatFn = function(biomass, params) params
    
    res = switch(model,
                 "fox"     =foxFn(     biomass,params),
                 "schaefer"=schaeferFn(biomass,params),
                 "pellat"  =pellatFn(  biomass,params),
                 stop("has to be either 'fox', 'schaefer' or 'pellat'"))
    
    return(res)}
   
   b2aParams(model(object),params(object))}
     
boundFn=function(object){
  
  schaefer2Logistic=function(x){FLPar()}
  fox2fox          =function(x){FLPar()}
  pellat2Genfit    =function(x){FLPar()}
  
  switch(model(object),
         pellat  =schaefer2Logistic(params(object)),
         fox     =fox2fox(          params(object)),
         schaefer=pellat2Genfit(    params(object)),
         gulland =stop("Gulland not available in ASPIC"),
         fletcher=stop("Gulland not available in ASPIC"),
         shepherd=stop("Gulland not available in ASPIC"))
  }
