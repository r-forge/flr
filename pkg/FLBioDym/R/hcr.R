setMethod( 'hcr', signature(object='FLBioDym'),
  function(object, Ftar=0.8, Btrig=0.75, Fmin=0.025, Blim=0.25,...){
      ## Reference Points
      Btrig=refpts(object)["bmsy"]*Btrig
      Blim =refpts(object)["bmsy"]*Blim
      Fmin =refpts(object)["fmsy"]*Fmin
      Ftar =refpts(object)["fmsy"]*Ftar
  
      ## HCR
      if (Blim>=Btrig) stop("Btrig must be greater than Blim")
      a= FLPar((Ftar-Fmin)/(Btrig-Blim))
      b= FLPar(Ftar-a*Btrig)
      
      ## Calc F
      SSB =stock(object)[,dims(object)$year]
      val =qmax(qmin(sweep(sweep(SSB,6,a,"*"),6,b,"+"),Ftar),Fmin)
  
      return(val)})


setMethod( 'TAC', signature(object='FLBioDym'),
  function(object,harvest,...){
  ## gets TAC
  yr    =dims(object)$year
  object=fwd(window(object,end=yr), harvest=harvest)

  return(catch(object)[,ac(yr)])})

