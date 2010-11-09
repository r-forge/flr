setGeneric("ggplot", function(data, ...)
	standardGeneric("ggplot"))

setMethod("ggplot", signature("FLQuants"), function(data, ...){
  ggplot(as.data.frame(data),...)})

setMethod("ggplot", signature("FLQuant"), function(data, ...){
  ggplot(as.data.frame(data),...)})

setMethod("ggplot", signature(data="FLComp"), function(data,...){
  ggplot(as.data.frame(data),...)})

whooow  <-function(x,fn,probs) as.data.frame(FLQuants(lapply(fn, function(fn,x) quantile(fn(x), probs=probs, na.rm=T), x=x)))

setMethod("plot", signature(x="FLStock", y="missing"),
 function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),
                       fn=list("SSB"       =ssb,
                               "Recruits"  =rec,
                               "Plus Group"=function(x) stock.n(x)[ac(dims(x)$max)],
                               "Fpg"       =function(x) harvest(x)[ac(dims(x)$max)],
                               "F2:5"      =function(x) apply(harvest(x)[ac(2:5)],2,mean))){

   res   <-whooow(x,fn,probs)

   p1<-ggplot(res)+ geom_line(aes(x=year,y=data,group=iter,size=iter,lty=iter)) +
                    scale_size_manual(    values=size, name="Quantile") +
                    scale_linetype_manual(values=lty , name="Quantile") +
                    expand_limits(y = 0)                                +
                    xlab("Year") + ylab("")                             +
                    facet

   print(p1)
   invisible(p1)})

setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x,probs=c(0.95,0.50,0.05),size=c(0.5,1.0,0.5),lty=c(2,1,2),facet=facet_wrap(~qname,scale="free"),
                       fn=list("SSB"       =ssb,
                               "Recruits"  =rec,
                               "Plus Group"=function(x) stock.n(x)[ac(dims(x)$max)],
                               "F10+"      =function(x) fav(x,ages=10:dims(x)$max),
                               "F2:5"      =function(x) apply(harvest(x)[ac(2:5)],2,mean))){

   res     <-ldply(x, whooow, fn=fn, probs=probs)
   res$.id <-factor(res$.id)
   res$iter<-factor(res$iter)

   p1<-ggplot(res)+ geom_line(aes(x=year,y=data,group=.id:iter,size=iter,col=.id,lty=iter)) +
                    scale_size_manual(    values=size, name="Quantile") +
                    scale_linetype_manual(values=lty , name="Quantile") +
                    expand_limits(y = 0)                                +
                    xlab("Year") + ylab("")                             +
                    facet

   print(p1)
   invisible(p1)})
################################################################################

#setMethod("ggplot", signature(data="FLComp"), function(data,flq=NULL,...){
#  ggplot(as.data.frame(x=data,row.names=flq),...)})

#setMethod("as.data.frame", signature(x="FLComp", row.names="character", optional="missing"),
#	function(x, row.names, optional){
#
#	 df<-function(x,slots,names=slots) {
#                    res       <-FLQuants(mlply(slots, function(x,fl) do.call(x,list(fl)), fl=x))
#                    names(res)<-slots
#
#                    return(as.data.frame(res))}
#
#		return(df(x,slots=row.names))})

