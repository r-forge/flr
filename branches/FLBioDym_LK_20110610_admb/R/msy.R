#### BMSY functions ############################################################
setGeneric( 'msy',    function(object,params,...) standardGeneric( 'msy'))
setGeneric('fmsy',    function(object,params,...) standardGeneric('fmsy'))
setGeneric('bmsy',    function(object,params,...) standardGeneric('bmsy'))
setGeneric('refpts',  function(object,params,...) standardGeneric('refpts'))
setGeneric('refptSE', function(object,...) standardGeneric('refptSE'))

setMethod('fmsy', signature(object='character',params="FLPar"),
   function(object,params,...){

    fmsyPellaT  <-function(params) params["r"]*(1/(1+params["p"]))
    fmsyFox     <-function(params) params["r"]*(1-(log(params["K"])-1)/log(params["K"]))
    fmsySchaefer<-function(params) params["r"]/2
    fmsyShepherd<-function(params) msyShepherd(params)/bmsyShepherd(params)
    fmsyGulland <-function(params) params["r"]*params["K"]/2
    fmsyFletcher<-function(params) msyFletcher(params)/bmsyFletcher(params)
    
    res<-switch(object,
               fox     =fmsyFox(     params),
               schaefer=fmsySchaefer(params),
               gulland =fmsyGulland( params),
               fletcher=fmsyFletcher(params),
               pellat  =fmsyPellaT(  params),
               shepherd=fmsyShepherd(params))

    return(res)})

setMethod('msy', signature(object='character',params="FLPar"),
   function(object,params,...){

    msyFox     <-function(params) params["r"]*(params["K"]*exp(-1))*(1-(log(params["K"])-1)/log(params["K"]))
    msySchaefer<-function(params) params["r"]*params["K"]/4
    msyPellaT  <-function(params) params["r"]*params["K"]*(1/(1+params["p"]))^(1/params["p"]+1)
    
    msyShepherd<-function(params){
         aPrime<-params["r"]/params["m"] - 1
         Bmax  <-params["K"]*aPrime
        .bmsy <- 0 #bmsy("shepherd",param)
    
        aPrime*m*.bmsy*(1-.bmsy/Bmax)/(1+aPrime)^.5}
    msyGulland  <-function(params) (params["r"]*params["K"]^2)/4
    msyFletcher <-function(params) params["msy"]

    res<-switch(object,
                   fox     =msyFox(     params),
                   schaefer=msySchaefer(params),
                   gulland =msyGulland( params),
                   fletcher=msyFletcher(params),
                   pellat  =msyPellaT(  params),
                   shepherd=msyShepherd(params))

    return(res)})

setMethod('bmsy', signature(object='character',params="FLPar"),
   function(object,params){
 
    bmsyFox     <-function(params) params["K"]*exp(-1)
    bmsySchaefer<-function(params) params["K"]/2
    bmsyPellaT  <-function(params) params["K"]*(1/(1+params["p"]))^(1/params["p"])
    bmsyShepherd<-function(params){
        aPrime<-params["r"]/params["m"] - 1
        Bmax  <-params["K"]*aPrime
    
        Bmax*((1+aPrime)^.5-1)/aPrime}
    bmsyGulland <-function(params) params["K"]/2
    bmsyFletcher<-function(params) params["K"]*(1/params["p"])^(1/(params["p"]-1))

    res<-switch(object,
         "fox"  =bmsyFox(      params) ,
         schaefer=bmsySchaefer(params),
         gulland =bmsyGulland( params),
         fletcher=bmsyFletcher(params),
         pellat  =bmsyPellaT(  params),
         shepherd=bmsyShepherd(params))

    return(res)})
    
setMethod('msy', signature(object="FLBioDym",params="missing"),
   function(object)  msy(model(object),params(object)))
setMethod('fmsy', signature(object="FLBioDym",params="missing"),
   function(object) fmsy(model(object),params(object)))
setMethod('bmsy', signature(object="FLBioDym",params="missing"),
   function(object) bmsy(model(object),params(object)))
   
#### ref pts
setMethod('refpts', signature(object='FLBioDym'),
   function(object){
     
     dmns<-dimnames(params(object))
     names(dmns)[1]<-"refpts"
     dmns[[1]]<-c("msy","fmsy","bmsy")
     res<-FLPar(NA,dimnames=dmns)
     
     res[ "msy"]<- msy(object)
     res["fmsy"]<-fmsy(object)
     res["bmsy"]<-bmsy(object)
     
     return(res)})

# if (!isGeneric("r"))
#    setGeneric( 'r',    function(m,fec,...) standardGeneric( 'r'))
#if (!isGeneric("vb"))
#    setGeneric('vb',    function(m,fec,...) standardGeneric( 'vb'))
# setMethod("r", signature(m="FLBioDym", fec="missing"),
#   function(m, fec,...) params(m)["r"])
#setMethod("vb", signature(m="FLBioDym", fec="missing"),
#   function(m, fec,...) params(m)["K"])

setMethod('refptSE', signature(object='FLBioDym'),
   function(object){
      rfpts<-c("msy","bmsy","fmsy")

      model<-model(object)

      nms <-unlist(parLst[model])[unlist(parLst[model]) %in% dimnames(vcov(object))[[1]]]

      cvr            <-vcov(object)[nms,nms,1]
      cvr[is.na(cvr)]<-0
      pars           <-params(object)[nms,1]
      nits<-1
      
      anma<-nms
      if ("p" %in% unlist(parLst[model])) anma<-c(anma,"p")
      if ("m" %in% unlist(parLst[model])) anma<-c(anma,"m")

      t.<-array(0,c(3,3,nits),dimnames=list(refpts=rfpts,refpts=rfpts,iter=1:nits))
      for (it in 1:nits){
        args=as.list(params(object)[anma,it,drop=T])
        print(args)
        for (i in rfpts)
          for (j in rfpts)
            for (k in  nms)
              for (l in  nms){

                 t.[i,j,it] = t.[i,j,it] + do.call(msyDeriv[[model]][[i]][[k]],args)*
                                           do.call(msyDeriv[[model]][[j]][[l]],args)*
                                           cvr[k,l]}}
                                           
      return(t.)})

