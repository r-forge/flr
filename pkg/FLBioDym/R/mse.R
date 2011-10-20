
mseFLBioDym<-function(OM,start,sr,srRsdl,
                      CV   =0.3,
                      Ftar =0.75,    Btrig=0.75,
                      Fmin =Ftar*0.1,Blim =Btrig*0.0,
                      Bpct =0.5,     Fpct =0.5,
                      jk    =FALSE,
                      bounds=NULL){
    
  ## Get number of iterations in OM
  nits  =dims(OM)$iter

  #### Observation Error (OEM) setup #######################
  ## Random variation for Catch & CPUE, CV=0.25%
  bd       =as(OM,"FLBioDym")
  bd       =propagate(bd,dims(object)$iter)
 
  index(bd)=index(bd)*rlnorm(prod(dim(index(bd))),0,CV)
  
  if (!is.null(bounds)) bd@bounds=bounds
    
  ## Loop round years
  for (iYr in start:(range(OM,"maxyear")-2)){
  #iYr = (start:(range(OM,"maxyear")-2))[1]
     cat("===================", iYr, "===================\n")
     ## add year to FLBD
     bd                 =window(bd,end=iYr)
     index(bd)[,ac(iYr)]=stock(OM)[,ac(iYr)]*rlnorm(prod(dim(index(bd)[,ac(iYr)])),0,CV)
     catch(bd)[,ac(iYr)]=computeCatch(OM)[,ac(iYr)]
 
     if (jk){
      hv =hcrJK(bd,Ftar,Btrig,Fmin,Blim,Fpct,Bpct) 
     }else{ 
       bd =admbBD(bd)
       hv =hcr(bd,Ftar,Btrig,Fmin,Blim) 
       }
     
     TAC=TAC(bd,hv)
       
     #### Now you have TAC take it from OM
     ctrl    <-fwdControl(data.frame(year=iYr+2,max=c(NA,2),quantity=c("catch","f")))
     dms     <-dimnames(ctrl@trgtArray)
     dms$iter<-1:nits
     ctrl@trgtArray<-array(NA,lapply(dms,length),dms)
     ctrl@trgtArray[1,"val", ]<-TAC
     ctrl@trgtArray[2,"max", ]<-0.35

     OM <-fwd(OM,ctrl=ctrl,sr=sr,sr.residuals=srRsdl)}

   return(OM)}
