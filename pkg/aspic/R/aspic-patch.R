df2FLPar=function(x) {
  #if names not==iter then a parameter
  
  dt  =x[,names(x)[names(x)!="iter"]]
  
  iters=1
  if ("iter" %in% names(x))
    iters=x$iter
  
  dmns=dimnames=list(iter=iters,params=names(x)[names(x)!="iter"])
  pars=FLPar(t(array(unlist(c(dt)),dim=unlist(lapply(dmns,length)),dimnames=dmns)))
  
  attributes(pars)$units=NA
  
  return(pars)}

setMethod("window", signature(x="aspic"),
          function(x, start=range(x,"minyear"), end=range(x,"maxyear"), extend=TRUE, frequency=1) {
            x <- qapply(x, window, start=start, end=end, extend=extend, frequency=frequency)
            x@range["minyear"] <- start
            x@range["maxyear"] <- end
           
            x@cpue=x@cpue[x@cpue$year %in% start:end,]
            
            return(x)})

setMethod('harvest', signature(object='aspic'),
          function(object) {
            res <- catch(object)/stock(object)[,dimnames(catch(object))$year]
            units(res) <- "hr"
            return(res)
          })



