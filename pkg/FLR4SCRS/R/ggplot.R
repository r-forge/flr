setGeneric("ggplot", function(data, ...)
	standardGeneric("ggplot"))

setMethod("ggplot", signature("FLQuants"), function(data, ...){
  ggplot(as.data.frame(data),...)})

setMethod("ggplot", signature("FLQuant"), function(data, ...){
  ggplot(as.data.frame(data),...)})

setMethod("as.data.frame", signature(x="FLComp", row.names="character", optional="missing"),
	function(x, row.names, optional){

	 df<-function(x,slots,names=slots) {
                    res       <-FLQuants(mlply(slots, function(x,fl) do.call(x,list(fl)), fl=x))
                    names(res)<-slots

                    return(as.data.frame(res))}

		return(df(x,slots=row.names))})

setMethod("ggplot", signature(data="FLComp"), function(data,flq,...){
  ggplot(as.data.frame(x=data,row.names=flq),...)})

#as.data.frame(FLBRP(ple4),c("catch","landings","discards"))