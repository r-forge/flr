setMethod( 'hcr', signature(object='FLBioDym'),
  function(object, Ftar=0.8, Btrig=0.75, Fmin=0.025, Blim=0.25,...){
      ## Reference Points
      Btrig=refpts(object)["bmsy"]*Btrig
      Blim =refpts(object)["bmsy"]*Blim
      Fmin =refpts(object)["fmsy"]*Fmin
      Ftar =refpts(object)["fmsy"]*Ftar
  
      ## HCR
      #if (Blim>=Btrig) stop("Btrig must be greater than Blim")
      a= FLPar((Ftar-Fmin)/(Btrig-Blim))
      b= FLPar(Ftar-a*Btrig)
      
      ## Calc F
      SSB =stock(object)[,dims(object)$year]
      val =qmax(qmin(sweep(sweep(SSB,6,a,"*"),6,b,"+"),Ftar),Fmin)
  
      return(val)})


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
  yr    =dims(object)$maxyear
  catch(object)=propagate(catch(object),dims(object)$iter)
  object=fwd(window(object,end=yr), harvest=harvest)

  return(catch(object)[,ac(yr)])})

