setMethod('diags',  signature(object='biodyn',method="missing"), function(object,method,...){
  object@diags
  })

setMethod('diags<-', signature(object='biodyn', value='data.frame'),
          function(object,value){
            object@diags=value
            return(object)})

