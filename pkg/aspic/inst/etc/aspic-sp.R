spFn=function(biomass,params,model="pellat") {

      schaeferFn = function(biomass, params) { 
           #logistic
           r=4*params["msy"]/params["k"]
            
           r*biomass*(1-biomass/params["k"])}
                   
    res = switch(model,
              "schaefer"=schaeferFn(biomass,params),
              "fox"     =foxFn(     biomass,params),
              "pellat"  =pellatFn(  biomass,params))
            
   return(res)}


plotSP <- function(object,biomass=FLQuant(seq(0,params(object)["K"],length.out=101))) {
  p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=spFn(object,biomass)))) +
    geom_line(aes(stock, yield)) +
    geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
    xlab("Stock") + ylab("Surplus Production")
  print(p)
  
  invisible(p)}

