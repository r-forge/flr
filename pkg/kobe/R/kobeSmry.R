setMethod('kobeSmry', signature(stock="numeric",harvest="numeric"),
    function(stock,harvest){
                                  
    res=kobeSmryFn(data.frame(stock=stock,harvest=harvest))
      
    return(res)})

setMethod('kobeSmry', signature(stock='data.frame',harvest="missing"),
    function(stock){
     
    res=kobeSmryFn(stock)
      
    return(res)})

kobeSmryFn=function(x){
  
  res =cbind(x,   kobeP(x$stock,x$harvest))
  res = with(res, data.frame(stock      =median(stock,       na.rm=T),
                             harvest    =median(harvest,     na.rm=T),
                             red        =mean(  red,         na.rm=T),
                             yellow     =mean(  yellow,      na.rm=T),
                             green      =mean(  green,       na.rm=T),
                             overFished =mean(  overFished,  na.rm=T),
                             overFishing=mean(  overFishing, na.rm=T)))
  
  return(res)}


