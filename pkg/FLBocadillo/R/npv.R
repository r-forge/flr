# npv	{{{
setGeneric('npv', function(object, ...)
		standardGeneric('npv')
)

setMethod('npv', signature('FLBRP'),
	function(object,d=0.05,dfg=NULL,gen=100){

     unts <-units(profit(object))

     NPG<-function(object,d,gen){
        res  <-profit(object)
        res[]<-0
        for (i in 1:gen)
           res<-res+profit(object)/(1+d)^i

        return(res)
        }

     NPGCon<-function(object,d=0.05,dfg=0.20,gen=100){
        res  <-profit(object)
        res[]<-0
        delta<-dfg/d

        for (i in 1:gen){
           wt <-d+dfg*d^(i-1)/gen*(1-delta^i)/(1-delta)
           print(delta)
           res<-wt^i*profit(object)
           }

        return(res)
        }

     if (is.null(dfg)) res<-NPG(   object,d,gen) else
                       res<-NPGCon(object,d,dfg,gen)

     units(res)<-unts
     
     return(res)
})

setMethod('npv', signature('FLStock'),
	function(object,price,vcost,fcost,d=0.05){

     res  <-profit(object)
     res[]<-0
     unts <-units(profit(object))

        rev   <-apply(landings.n(object)*price,c(2:6),sum)
        costs <-fcost+fbar(objects)*vcost
        profit<-rev-profit
        wt    <-FLQuant(cumprod(rep(1+0.05,100)),dimnames=list(year=2008:2107))
        res   <-profit/wt
        
     for (i in 1:gen){
        p<-i-as.integer(i/IG)*IG
        if (p==0) p<-IG

        res<-res+profit(object)/(1+d)^p
        }

     units(res)<-unts
     return(res)
})

npv<-function(object,price,vcost,fcost,d){
  rev   <-apply(landings.n(object)*price,c(2:6),sum)
  costs <-fcost+fbar(object)*vcost
  profit<-rev-costs
  wt    <-FLQuant(cumprod(rep(1+d,100)),dimnames=list(year=2008:2107))
  res   <-profit/wt
  res   <-apply(res,1,sum)
  
  return(res)
  }
