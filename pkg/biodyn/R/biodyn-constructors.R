#' biodyn Class
#'
#' @description Creates an object of the biodyn class representing a biomass dynamic stock assessment model.
#' @name biodyn
#' @param model a factor or string that specifies the model type, has to be one of "fox", "schaefer", "pellat", "gulland", "fletcher", "shepherd", "logistic", "genfit"
#' @param params model parameters
#' @return biodyn object
#' @export
#' @examples bd=biodyn("logistic",FLPar(k=50000,msy=1000,b0=1))
setGeneric('biodyn',   function(model,params,...)  standardGeneric('biodyn'))
setMethod('biodyn', signature(model='factor',params="FLPar"),
          function(model,params,...){
            
            args = list(...)
            
            res=biodyn()
            res@model =model
            res@params=params 
            res@stock[]=params(res)["k"]
            res@catch[]=0
            
            # Load given slots
            for(i in names(args))
              slot(res, i) = args[[i]]
            
            return(res)})

setMethod('biodyn', signature(model='character',params="FLPar"),
          function(model,params,...) biodyn(model=factor(model,levels=biodyn:::models),params))

setMethod('biodyn', signature(model='missing',params="missing"),
          function(model,params,...) {
            args = list(...)
            
            res=new("biodyn")
            
            # Load given slots
            for(i in names(args))
              slot(res, i) = args[[i]]
            
          return(res)})

#' Checks class type
#'
#' @description Returns TRUE if object is of type biodyn
#' @param x biodyn class
#' @return TRUE or FALSE
#' @export
#' @examples
#' is.biodyn(biodyn()) 
is.biodyn = function(x)
  return(inherits(x, "biodyn"))
