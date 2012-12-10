setMethod("plot", signature(x="biodyn", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComp(x,fn,probs,size,lty,facet))

setMethod("plot", signature(x="biodyns", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComps(x,fn,probs,size,lty,facet))

plotSP=function(object,biomass=FLQuant(seq(0,params(object)["k"],length.out=101))) {
   p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=computeSP(object,biomass)))) +
             geom_line(aes(stock, yield)) +
             geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
             xlab("Stock") + ylab("Surplus Production")
   print(p)
   invisible(p)} 
