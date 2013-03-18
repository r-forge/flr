hcr.=  function(object, params=FLPar(Ftar=0.8,  Btrig=0.75, Fmin=0.025, Blim=0.25),
                        msy   =    c(Ftar=TRUE, Btrig=TRUE, Fmin=TRUE,  Blim=TRUE),
                        refpt ="missing",
                        bndTAC=NULL, bndF=NULL,lag=1,...){

      if (dims(params)$iter==1 & dims(refpts(object))$iter>1)
         params=propagate(params,dims(refpts(object))$iter)

      ## Reference Points
      if (refpt=="missing")
         refpt=refpts(object)
      
      if (msy["Btrig"]) params["Btrig"]=refpt["bmsy"]*params["Btrig"]
      if (msy["Blim"])  params["Blim"] =refpt["bmsy"]*params["Blim"]
      if (msy["Fmin"])  params["Fmin"] =refpt["fmsy"]*params["Fmin"]
      if (msy["Ftar"])  params["Ftar"] =refpt["fmsy"]*params["Ftar"]

      ## HCR
      #if (Blim>=Btrig) stop("Btrig must be greater than Blim")
      a=(params["Ftar"]-params["Fmin"])/(params["Btrig"]-params["Blim"])
      b= params["Ftar"]-a*params["Btrig"]
 
      ## Calc F
      #SSB =apply(stock(object)[,ac(as.numeric(dims(object)$year-lag))],6,sum)
      yrTAC=dims(catch(object))$maxyear+1
      yrRef=ac(yrTAC-lag)
      yrTAC=ac(yrTAC)
 
      SSB =apply(stock(object)[,yrRef],6,sum)
      val=sweep(sweep(SSB,6,a,"*"),6,b,"+")
      for (i in seq(dim(val)[6])){
         val[,,,,,i]=max(val[,,,,,i],params["Fmin",i])
         val[,,,,,i]=min(val[,,,,,i],params["Ftar",i])}

      dimnames(val)$year=yrTAC
      #hrRef <- catch(object[,yrRef])/stock(object[,yrRef])
	  #if(val<hrRef*(1-bndF)) val <- hrRef*(1-bndF)
	  #if(val>hrRef*(1+bndF)) val <- hrRef*(1+bndF)
  
      return(val)}

setMethod('hcr', signature(object='FLBioDym'),
           function(object, params=FLPar(Ftar=0.8,  Btrig=0.75, Fmin=0.025, Blim=0.25),
                             msy  =    c(Ftar=TRUE, Btrig=TRUE, Fmin=TRUE,  Blim=TRUE),...) 
   hcr.(object,params,msy,...))

setGeneric('hcrJK', function(object, ...) standardGeneric('hcrJK'))
setMethod( 'hcrJK', signature(object='FLBioDym'),
  function(object,Ftar=0.8,Btrig=0.75,Fmin=0.025,Blim=0.25,Fpct=0.35,Bpct=0.85,...){
    
    #Ftar=0.8;Btrig=0.75;Fmin=0.025;Blim=0.25;Fpct=0.75;Bpct=0.75
      val=NULL
      
      ## Jacknife
      for (i in 1:max(dims(catch(object))$iter,dims(index(object))$iter)){
         object.        =iter(object,i)
         index(object.) =jacknife(index(object.))
         object.        =admbBD(object.)
         
         ## Reference Points
         rp =refpts(object.)
         rp  =model.frame(FLQuants(llply(jackSummary(rp),FLQuant)))
         Fmsy=qnorm(Fpct,rp[2,"mean"],rp[2,"se"])
         Bmsy=qnorm(Bpct,rp[3,"mean"],rp[3,"se"])
         
         Btrig.=Bmsy*Btrig
         Blim. =Bmsy*Blim
         Fmin. =Fmsy*Fmin
         Ftar. =Fmsy*Ftar         
      
         ## HCR
         #if (Blim>=Btrig) stop("Btrig must be greater than Blim")
         a= FLPar((Ftar.-Fmin.)/(Btrig.-Blim.))
         b= FLPar(Ftar.-a*Btrig.)
      
         ## Calc F
         SSB    =stock(iter(object.,1))[,dims(object.)$year-1]
         val=c(val, qmax(qmin(sweep(sweep(SSB,6,a,"*"),6,b,"+"),Ftar.),Fmin.))         
         }
      
      dmns=dimnames(SSB)
      dmns$year=as.numeric(dmns$year)+1
      dmns$iter=1:length(val)
      val =FLQuant(val,dimnames=dmns)
      return(val)})

setMethod( 'TAC', signature(object='FLBioDym'),
function(object,harvest,...){
  ## gets TAC
  yr    =dims(harvest)$maxyear
  catch(object)=propagate(catch(object),dims(object)$iter)
  object=fwd(window(object,end=yr), harvest=harvest)

  return(catch(object)[,ac(yr)])})

