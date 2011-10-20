setMethod('kobe', signature(object='FLBioDym'),
  function(object,xlim=c(0,2),ylim=xlim){
    
    res=model.frame(FLQuants("biomass"=stock(object)/bmsy(object),
                             "harvest"=harvest(object)/fmsy(object)),drop=TRUE)    
    invisible(kobe(res,xlim,ylim))})

