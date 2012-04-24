setMethod('kobe', signature(object='FLBioDym'),
  function(object,lim=c(0,2),xlim=lim,ylim=xlim){
      
  res=model.frame(mcf(FLQuants(stock  =sweep(stock(  object),6,bmsy(object),"/"),
                               harvest=sweep(harvest(object),6,fmsy(object),"/"),
                               yield  =sweep(catch(  object),6, msy(object),"/"))),drop=TRUE)
  
  res=cbind(res,kobeP(res$stock,res$harvest))
  
  invisible(kobe(res,xlim,ylim))})

