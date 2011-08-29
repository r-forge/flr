#### Production functions ######################################################
setGeneric('sp', function(object,bio,params,...)
		standardGeneric('sp'))
		
setMethod('sp', signature(object='character'),
  function(object,bio,params,...){

    fox     <-function(bio,params)
      params["r"]*bio*(1-log(bio)/log(params["K"]))
    schaefer<-function(bio,params)    params["r"]*bio*(1-bio/params["K"])
    pellat  <-function(bio,params)    params["r"]/params["p"]*bio*(1-(bio/params["K"])^params["p"])
    shepherd<-function(bio,params)    params["r"]*bio/(1+bio/params["K"])-params["m"]*bio
    gulland <-function(bio,params)    params["r"]*bio*(params["K"]-bio)
    fletcher<-function(bio,params){
        lambda<-(params["p"]^(params["p"]/(params["p"]-1)))/(params["p"]-1)

        lambda*msy*(bio/params["K"])-lambda*params["msy"]*(bio/params["K"])^params["p"]}

    res<-switch(object,
           fox     =fox(     bio,params),
           schaefer=schaefer(bio,params),
           gulland =gulland( bio,params),
           fletcher=fletcher(bio,params),
           pellat  =pellat(  bio,params),
           shepherd=shepherd(bio,params))

    return(res)})
    
setMethod('sp', signature(object='FLBioDym'),
  function(object,bio) sp(model(object),bio,params(object)))
  
  
  
