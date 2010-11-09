## Diagnostics
setGeneric('diags', function(object, ...)
	standardGeneric('diags'))

setMethod("diags", signature(object="FLXSA"),
    function(object,i=NULL){
   
       fn<-function(object,i){
           x    =index(    object)[[i]]
           yHat =index.hat(object)[[i]]
           rsdl =index.res(object)[[i]]

           dmns<-dimnames(x)
           y    =stock.n(  object)[dmns$age,dmns$year]

           rsdlLag      =FLQuant(NA,dimnames=dimnames(rsdl))
           rsdlLag[,-dim(rsdl)[2]]=rsdl[,-1]
           qq.          =qqnorm(c(rsdl),plot.it=FALSE)
           qqx          =FLQuant(qq.$x,dimnames=dimnames(rsdl))
           qqy          =FLQuant(qq.$y,dimnames=dimnames(rsdl))

           res<-model.frame(FLQuants(x=x,y=y,yHat=yHat,rsdl=rsdl,rsdlLag=rsdlLag,qqx=qqx,qqy=qqy))

           return(res)}

    if (is.null(i)){
      nms<-index.name(object)
      res<-mdply(data.frame(i=1:length(nms)), function(x,p) data.frame(index=nms[x],fn(p,x)), object)
    }else
      res<-fn(object,i)

    return(res)})

setMethod("diags", signature(object="FLSR"),
    function(object,i=NULL){

           x    =ssb(      object)
           yHat =predict(  object)
           rsdl =residuals(object)

           dmns<-dimnames(x)
           y    =rec(object)

           rsdlLag      =FLQuant(NA,dimnames=dimnames(rsdl))
           rsdlLag[,-dim(rsdl)[2]]=rsdl[,-1]
           qq.          =qqnorm(c(rsdl),plot.it=FALSE)
           qqx          =FLQuant(qq.$x,dimnames=dimnames(rsdl))
           qqy          =FLQuant(qq.$y,dimnames=dimnames(rsdl))

           ssb  =FLQuant(seq(0,max(x,na.rm=T),length.out=dim(x)[2]),dimnames=dimnames(x))
           rec  =predict(  object, ssb=ssb)

           res<-model.frame(FLQuants(x=x,y=y,yHat=yHat,rsdl=rsdl,rsdlLag=rsdlLag,qqx=qqx,qqy=qqy,rec=rec,ssb=ssb))


    return(res)})
