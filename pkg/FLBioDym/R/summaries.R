jacknife.summary<-function (flx) {
    nms     <-names(dimnames(flx))
    idx     <-seq(length(nms))[nms != 'iter']
    n       <-dims(flx)$iter-1
      
    mn      <-iter(flx,  1)
    u       <-iter(flx, -1)
    mnU     <-apply(u,idx,mean)   
  
    SS      <-apply(sweep(u,idx, mnU,"-")^2,idx,sum)
  
    bias<-(n - 1) * (mnU - mn)
    se  <-sqrt(((n-1)/n)*SS)
  
    return(list("1"=mn,mean=mnU,se=se,bias=bias))}
  
smryStats<-function(bd){                 
    stk   =as.data.frame(stock(  bd)[,dims(bd)$year-1])[,6:7]
    hvt   =as.data.frame(catch(bd)[,dims(bd)$year-1]/stock(bd)[,dims(bd)$year-1])[,6:7]
    stkRel=as.data.frame(stock(bd)[,dims(bd)$year-1]/c(refpts(bd)["bmsy"]))[,6:7]
    hvtRel=as.data.frame((catch(bd)[,dims(bd)$year-1]/stock(  bd)[,dims(bd)$year-1])/c(refpts(bd)["fmsy"]))[,6:7]
    rps   =as.data.frame(refpts(bd))
    par   =as.data.frame(params(bd))
    
    names(rps)[1]="params"
    names(par)[1]="params"

    res<-rbind(rps,par,
               data.frame(params="stock",     stk),
               data.frame(params="harvest",   hvt),
               data.frame(params="stockMSY",  stkRel),
               data.frame(params="harvestMSY",hvtRel))
 
    return(as.FLQuant(res))}
