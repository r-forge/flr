###########################################################################################################################3
# Advice plots Brydes Stylie
#  
# Wish list
#   1) drop=T for as.data.frame
#   2) add biomass.obs & revenue.obs
#   3) sr<- for passing sr(brp) to other objects 
###########################################################################################################################3

#library(FLAdvice)
#data(ple4)
#x<-brp(FLBRP(ple4))
#params(x)[]<-exp(mean(log(rec.obs(x)),na.rm=T))
#x<-brp(x)

setMethod("plot", signature(x="FLBRP", y="missing"),
 function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),scale="msy",
                       fn=list("SSB"    =function(x) ssb.obs(  x)/refpts(x)[scale,    "ssb"],
				"yield"  =yield.obs(x)/refpts(x)[scale,  "yield"],
				"rec"    =rec.obs(  x)/refpts(x)[scale,    "rec"],
				"harvest"=fbar.obs( x)/refpts(x)[scale,"harvest"]),...) 
			        #biomass =stock.obs(  x)/refpts(x)[scale,"biomass"],
			        #profit  =profit.obs( x)/refpts(x)[scale,"profit"],
			        #revenue =revenue.obs(x)/refpts(x)[scale,"revenue"],
    plotComp(x,fn,probs,size,lty,facet))

setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),
                       fn=list("SSB"    =function(x) ssb.obs(  X)/refpts(x)[scale,    "ssb"],
				"yield"  =yield.obs(x)/refpts(x)[scale,  "yield"],
				"rec"    =rec.obs(  x)/refpts(x)[scale,    "rec"],
				"harvest"=fbar.obs( x)/refpts(x)[scale,"harvest"]),...) 
			        #biomass =stock.obs(  x)/refpts(x)[scale,"biomass"],
			        #profit  =profit.obs( x)/refpts(x)[scale,"profit"],
			        #revenue =revenue.obs(x)/refpts(x)[scale,"revenue"],

    plotComps(x,fn,probs,size,lty,facet))


##stats
# short-, medium- and long-tern 
# diacount rates
# stock conservation
# food sustainanility
# economic

