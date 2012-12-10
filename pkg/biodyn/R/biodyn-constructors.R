setMethod('biodyn', signature(model='factor',params="FLPar"),
    function(model,params,...){
      
      args = list(...)
    
      res=biodyn()
      res@model =model
      res@params=params 
      
      # Load given slots
      for(i in names(args))
        slot(res, i) = args[[i]]
      
      return(res)})

setMethod('biodyn', signature(model='character',params="FLPar"),
          function(model,params,...) biodyn(model=as.factor(model,levels=models),params))

is.biodyn = function(x)
	return(inherits(x, "biodyn"))
