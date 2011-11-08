#### Plots ##########################################################################################

setMethod('plot', signature(x='FLsz',y="missing"),
    function(x,y,...){
    p.=ggplot(merge(getHat(x),as.data.frame(ln2z(x),drop=T)))+
                geom_point( aes(year,data))  +
                geom_smooth(aes(year,data))  +
                expand_limits(y = 0) +
                theme_flr(size=12.5)   +
                scale_y_continuous(name="Z") +
                scale_x_continuous(name="Year") +
                geom_line(aes(year,z,   group=bit)) +
                geom_line(aes(year,z+sd,group=bit),colour="red") + 
                geom_line(aes(year,z-sd,group=bit),colour="red")
    
    print(p.)
    invisible(p.)})


plots.=function(x,...){
   p.=ldply(x,function(x) plot(x)$data) 
   p..=ggplot(p.)+
                geom_point( aes(year,bhz,group=.id))    +
                geom_smooth(aes(year,bhz,group=.id))    +
                expand_limits(y = 0)                    +
                theme_flr(size=12.5)                    +
                scale_y_continuous(name="Z")            +
                scale_x_continuous(name="Year")         +
                geom_line(aes(year,z,   group=factor(bit):factor(.id))) +
                geom_line(aes(year,z+sd,group=factor(bit):factor(.id)),colour="red") + 
                geom_line(aes(year,z-sd,group=factor(bit):factor(.id)),colour="red")
    
    print(p..)
    invisible(p..)}
    
setGeneric('diags', function(object,...)
   standardGeneric('diags'))
setMethod("diags", signature(object="FLsz"),
  function(object, i=NULL) {
    
    res=cbind(getHat(object),
              model.frame(FLQuants(y=object@obs,yHat=object@hat,residual=object@residuals),drop=T)[,-1])
    
    res$residualLag <-c(residuall[-1],NA)

    qq. <- qqnorm(c(residual),plot.it=FALSE)
    qqx <- FLQuant(qq.$x,dimnames=dmns)
    qqy <- FLQuant(qq.$y,dimnames=dmns)

    res <- model.frame(FLQuants(x=x, y=y, yHat=yHat, residual=residual, residualLag=residualLag, qqx=qqx, qqy=qqy))

    return(res)})
