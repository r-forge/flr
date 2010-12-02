#### BMSY functions ############################################################
setGeneric('refpts', function(object,...)
		standardGeneric('refpts'))

setGeneric('refptSE', function(object,...)
		standardGeneric('refptSE'))

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
    nms<-dimnames(params(object))$params
    if ("r"   %in% nms) r  =params(object)["r",]   else r=  NULL
    if ("K"   %in% nms) K  =params(object)["K",]   else K=  NULL
    if ("m"   %in% nms) m  =params(object)["m",]   else m=  NULL
    if ("p"   %in% nms) p  =params(object)["p",]   else p=  NULL
    if ("msy" %in% nms) msy=params(object)["msy",] else msy=NULL
    res<-msy(model(object),r=r,K=K,m=m,p=p,msy=msy)

    dms<-dimnames(res)
    dimnames(res)<-list(refpt="msy",dms$iter)
    
    return(res)
    })

setMethod('bmsy', signature(object='FLBioDym'),
   function(object){
    nms<-dimnames(params(object))$params
    if ("r"   %in% nms) r=params(object)["r",]     else r=NULL
    if ("K"   %in% nms) K=params(object)["K",]     else K=NULL
    if ("m"   %in% nms) m=params(object)["m",]     else m=NULL
    if ("p"   %in% nms) p=params(object)["p",]     else p=NULL
    if ("msy" %in% nms) msy=params(object)["msy",] else msy=NULL
    res<-bmsy(model(object),r=r,K=K,m=m,p=p)
    
    dms<-dimnames(res)
    dimnames(res)<-list(refpt="bmsy",dms$iter)

    return(res)
    })

setMethod('fmsy', signature(object='FLBioDym'),
   function(object){
   
    nms<-dimnames(params(object))$params

    if ("r"   %in% nms) r=params(object)["r",]     else r=NULL
    if ("K"   %in% nms) K=params(object)["K",]     else K=NULL
    if ("m"   %in% nms) m=params(object)["m",]     else m=NULL
    if ("p"   %in% nms) p=params(object)["p",]     else p=2
    if ("msy" %in% nms) msy=params(object)["msy",] else msy=NULL
    res<-fmsy(model(object),r=r,K=K,m=m,msy=msy,p=p)

    dms<-dimnames(res)
    dimnames(res)<-list(refpt="fmsy",dms$iter)

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
    #K*(1+p)^(-1/p)
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
    r*((K/p)^(1/(p-1)))-r/K*((K/p)^(p/(p-1)))}
    #K*(1+p)^(-1/p)*(1-(1+p)^(-1/p))}

msyShepherd<-function(r,K,m){
     aPrime<-r/m - 1
     Bmax  <-K*aPrime
    .bmsy <-bmsy("shepherd",r,K,m)

    aPrime*m*.bmsy*(1-.bmsy/Bmax)/(1+aPrime)^.5
    }

msyGulland <-function(r,K){
    (r*K^2)/4}

msyFletcher <-function(msy,p=2){
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
    #msy("pellat",r,K,p=p)/bmsy("pellat",r,K,p=p)}
    #r*(1-(1+p)^(-1/p))}

    msy("pellat",r,K,p=p)/bmsy("pellat",r,K,p=p)
    }

fmsyShepherd<-function(r,K,m){
    msyShepherd(r,K,m)/bmsyShepherd(r,K,m)
    }

fmsyGulland <-function(r,K){
    (r*K)/2}

fmsyFletcher <-function(K,msy,p=2){
    msyFletcher(msy,p)/bmsyFletcher(K,p)}

if (!isGeneric("computeRefpts"))
	setGeneric("computeRefpts", function(object, ...)
		standardGeneric("computeRefpts"))

#### ref pts
setMethod('computeRefpts', signature(object='FLBioDym'),
   function(object,SE=F)
     {
     if (SE)
        return(refptsSE(object))
     
     res<-array(NA,c(3,dims(object)$iter),list(msy=c("catch","stock","harvest"),iter=1:dims(object)$iter))
     res["harvest",]<-fmsy(object)
     res["stock",]  <-bmsy(object)
     res["catch",]  <-msy( object)

     return(res)
     })

setMethod('r', signature(m='FLBioDym',fec="missing"),

  function(m){
           object<-m
    res<-switch(model(object),
           fox     =return(params(object)["r",]),
           gulland =return(params(object)["r",]),
           fletcher=return(params(object)["r",]),
           pellat  =return(params(object)["r",]),
           shepherd=return(params(object)["r",]),
           return(NULL))
           
    return(res)
    })

setMethod('vb', signature(object='FLBioDym'),
  function(object){
    res<-switch(model(object),
           fox     =return(params(object)["K",]),
           schaefer=return(params(object)["K",]),
           gulland =return(params(object)["K",]),
           fletcher=return(params(object)["K",]),
           pellat  =return(params(object)["K",]^1/((params(object)["p",]-1))),
           shepherd=return(),
           return(NULL))

    return(res)
    })

setMethod('refptSE', signature(object='FLBioDym'),
   function(object){
      rfpts<-c("msy","bmsy","fmsy")

      model<-model(object)

      #dignl<-expand.grid(par1=parLst[[model]],par2=parLst[[model]],rfpt=rfpts)[,c("par1","par2","rfpt")]
      #trngl<-expand.grid(rfpt1=rfpts,rfpt2=rfpts,par1=parLst[[model]],par2=parLst[[model]])
      #trngl<-trngl[trngl[,"rfpt1"]!=trngl[,"rfpt2"],]

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
