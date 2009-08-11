#### BMSY functions ############################################################
setGeneric('refpts', function(object,...)
		standardGeneric('refpts'))

#### Reference points
setGeneric('refptsSE', function(object,...)
		standardGeneric('refptsSE'))

setGeneric('msy', function(object,...)
		standardGeneric('msy'))

setGeneric('bmsy', function(object,...)
		standardGeneric('bmsy'))

setGeneric('fmsy', function(object,...)
		standardGeneric('fmsy'))

setMethod('msy', signature(object='character'),
  function(object,r=0.5,K=0,m=0,p=2,msy=0){
    res<-switch(object,,
           fox     =msyFox(     r=r,K=K),
           schaefer=msySchaefer(r=r,K=K),
           gulland =msyGulland( r=r,K=K),
           fletcher=msyFletcher(    msy=msy),
           pellat  =msyPellaT(  r=r,K=K,p=p),
           shepherd=msyShepherd(r=r,K=K,m=m))

    return(res)
    })

setMethod('bmsy', signature(object='character'),
  function(object,r=0,K=0,m=0,p=2){
    res<-switch(object,
           fox     =bmsyFox(     r=r,K=K),
           schaefer=bmsySchaefer(r=r,K=K),
           gulland =bmsyGulland( r=r,K=K),
           fletcher=bmsyFletcher(    K=K,p=p),
           pellat  =bmsyPellaT(      K=K,p=p),
           shepherd=bmsyShepherd(r=r,K=K,m=m))

    return(res)
    })

setMethod('fmsy', signature(object='character'),
   function(object,r=0,K=0,m=0,p=2,msy=0){
    res<-switch(object,
           fox     =fmsyFox(     r=r,K=K),
           schaefer=fmsySchaefer(r=r,K=K),
           gulland =fmsyGulland( r=r,K=K),
           fletcher=fmsyFletcher(    K=K,msy=msy,p=p),
           pellat  =fmsyPellaT(  r=r,K=K,p=p),
           shepherd=fmsyShepherd(r=r,K=K,m=m))

    return(res)
    })

setMethod('msy', signature(object='FLBioDym'),
  function(object){
    nms<-names(params(object))
    if ("r"   %in% nms) r  =params(object)["r",]   else r=  NULL
    if ("K"   %in% nms) r  =params(object)["K",]   else K=  NULL
    if ("m"   %in% nms) r  =params(object)["m",]   else m=  NULL
    if ("p"   %in% nms) r  =params(object)["p",]   else p=  NULL
    if ("msy" %in% nms) msy=params(object)["msy",] else msy=NULL

    res<-msy(model(object),r=r,K=K,m=m,p=p,msy=msy)

    return(res)
    })

setMethod('bmsy', signature(object='FLBioDym'),
   function(object){

    nms<-names(params(object))
    if ("r" %in% nms) r=params(object)["r",] else r=NULL
    if ("K" %in% nms) r=params(object)["K",] else K=NULL
    if ("m" %in% nms) r=params(object)["m",] else m=NULL
    if ("p" %in% nms) r=params(object)["p",] else p=NULL

    res<-bmsy(model(object),r=r,K=K,m=m,p=p)
    
    return(res)
    })

setMethod('fmsy', signature(object='FLBioDym'),
   function(object){

    nms<-names(params(object))
    if ("r" %in% nms) r=params(object)["r",] else r=NULL
    if ("K" %in% nms) r=params(object)["K",] else K=NULL
    if ("m" %in% nms) r=params(object)["m",] else m=NULL
    if ("p" %in% nms) r=params(object)["p",] else p=NULL

    res<-fmsy(model(object),r=r,K=K,m=m,p=p)

    return(res)
    })

#BMSY functions
bmsyFox     <-function(r,K){
    K*exp(-1)
    }

#deriv(~r*bio*(1-bio/K),"bio")
bmsySchaefer<-function(r,K){
    K/2
    }

#deriv(~r*bio-r/K*bio^p,"bio")
bmsyPellaT  <-function(r,K,p=2){
    (K/p)^(1/(p-1))
    }

#deriv(~r*B/(1+bio/K)-m,"bio")
bmsyShepherd<-function(r,K,m){
    aPrime<-r/m - 1
    Bmax  <-K*aPrime

    Bmax*((1+aPrime)^.5-1)/aPrime
    }

bmsyGulland <-function(r,K){
    K/2
    }

bmsyFletcher <-function(K,p=2){
    K*(1/p)^(1/(p-1))
    }


#### MSY functions
msyFox     <-function(r,K){
    r*(K*exp(-1))*(1-(log(K)-1)/log(K))
    }

msySchaefer<-function(r,K){
    r*K/4
    }

msyPellaT  <-function(r,K,p=2){
    .bmsy<-bmsy("pellat",r=r,K=K,p=p)
    r*.bmsy-r/K*.bmsy^p
    }

msyShepherd<-function(r,K,m){
    aPrime<-r/m - 1
    Bmax  <-K*aPrime
    .bmsy <-bmsy("shepherd",r,K,m)

    aPrime*m*.bmsy*(1-.bmsy/Bmax)/(1+aPrime)^.5
    }

msyGulland <-function(r,K){
    (r*K^2)/4}

msyFletcher <-function(msy){
    msy
    }

#### FMSY functions
.fmsyFox     <-function(r,K){
    .bmsy<-K*exp(-1)

    (r*.bmsy*(1-log(.bmsy)/log(K)))/.bmsy
    }

fmsyFox     <-function(r,K){
    r*(1-(log(K)-1)/log(K))
    }

fmsySchaefer<-function(r,K){
    r/2}

fmsyPellaT  <-function(r,K,p=2){
    msy("pellat",r,K,p=p)/bmsy("pellat",r,K,p=p)
    }

fmsyShepherd<-function(r,K,m){
    msyShepherd(r,K,m)/bmsyShepherd(r,K,m)
    }

fmsyGulland <-function(r,K){
    (r*K)/2}

fmsyFletcher <-function(K,msy,p){
    msyFletcher(msy)/bmsyFletcher(K,p)
    }

#### ref pts
setMethod('refpts', signature(object='FLBioDym'),
   function(object)
     {
     res<-array(NA,c(3,dims(object)$iter),list(msy=c("harvest","stock","catch"),iter=1:dims(object)$iter))
     res["harvest",]<-fmsy(object@params["r",],object@params["K",],object@params["mpar",])
     res["stock",]  <-bmsy(                    object@params["K",],object@params["mpar",])
     res["catch",]  <-msy.( object@params["r",],object@params["K",],object@params["mpar",])

     return(res)
     })

setMethod('refptsSE', signature(object='FLBioDym'),
   function(object)
     {
     #### bmsy
     bmsySE<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dBdK(K,mpar)^2*cvr["K",   "K",   ]+
               dBdM(K,mpar)^2*cvr["mpar","mpar",]+

               dBdK(K,mpar)*dBdM(K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     #### msy
     msySE.<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dMdR(r,K,mpar)^2*cvr["r",   "r",   ]+
               dMdK(r,K,mpar)^2*cvr["K",   "K",   ]+
               dMdM(r,K,mpar)^2*cvr["mpar","mpar",]+

               dMdR(r,K,mpar)*dMdK(r,K,mpar)*cvr["r","K",   ]+
               dMdR(r,K,mpar)*dMdM(r,K,mpar)*cvr["r","mpar",]+
               dMdK(r,K,mpar)*dMdM(r,K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     #### fmsy
     fmsySE<-function(object){
          cvr            <-covar(object)
          cvr[is.na(cvr)]<-0
          r              <-object@params["r",   ]
          K              <-object@params["K",   ]
          mpar           <-object@params["mpar",]

          res<-dFdR(r,K,mpar)^2*cvr["r",   "r",   ]+
               dFdK(r,K,mpar)^2*cvr["K",   "K",   ]+
               dFdM(r,K,mpar)^2*cvr["mpar","mpar",]+

               dFdR(r,K,mpar)*dFdK(r,K,mpar)*cvr["r","K",   ]+
               dFdR(r,K,mpar)*dFdM(r,K,mpar)*cvr["r","mpar",]+
               dFdK(r,K,mpar)*dFdM(r,K,mpar)*cvr["K","mpar",]

         return(res^0.5)
         }

     res            <-array(NA,c(3,dims(object)$iter),list(c("harvest","stock","catch")))
     res["harvest",]<-fmsySE(object)
     res["stock",]  <-bmsySE(object)
     res["catch",]  <-msySE.(object)

     return(res)
     })

setMethod('r', signature(object='FLBioDym'),
  function(object){
    res<-switch(object,,
           fox     =return(),
           schaefer=return(),
           gulland =return(),
           fletcher=return(),
           pellat  =return(),
           shepherd=return(),
           return(NULL))
           
    return(res)
    })

setMethod('vb', signature(object='FLBioDym'),
  function(object){
    res<-switch(object,,
           fox     =return(params(object)["K",]),
           schaefer=return(params(object)["K",]),
           gulland =return(params(object)["K",]),
           fletcher=return(params(object)["K",]),
           pellat  =return(params(object)["K",]^1/((params(object)["p",]-1))),
           shepherd=return(),
           return(NULL))

    return(res)
    })
