#### Plots ##########################################################################################

setMethod('plot', signature(x='FLsz',y="missing"),
    function(x,y,...){
    p.=ggplot(merge(getHat(x),as.data.frame(ln2z(x),drop=T)))+
                geom_point( aes(year,data,group=iter))  +
               #expand_limits(y = 0) +
                theme_flr(size=12.5)   +
                scale_y_continuous(name="Z") +
                scale_x_continuous(name="Year") +
                geom_line(aes(year,z,   group=bit+iter*10000)) +
                geom_line(aes(year,z+sd,group=bit+iter*10000),colour="red") + 
                geom_line(aes(year,z-sd,group=bit+iter*10000),colour="red") +
                geom_smooth(aes(year,data,group=iter),se=FALSE)
 
    
    print(p.)
    invisible(p.)})


setGeneric('diags', function(object,...)
   standardGeneric('diags'))
setMethod("diags", signature(object="FLsz"),
  function(object,plotIt=TRUE) {
  
    rs1=getHat(object)
    rs2=model.frame(FLQuants(y=object@obs,yHat=object@hat,residual=object@residuals))[,c("year","iter","y","yHat","residual")]
    res=merge(rs1,rs2,by=c("year","iter"))  
           
    rsdls=function(x){       
      x$residualLag <-c(x$residual[-1],NA)

      qq.  = qqnorm(c(x$residual),plot.it=FALSE)
      x$qqx= qq.$x
      x$qqy= qq.$y

      return(x)}
           
    res=ddply(res, .(iter), rsdls)[,c("year","bit","iter","sd","y","yHat","residual","residualLag","qqx","qqy")]

    
    if (!plotIt) return(res)
    
    tmp1=cbind(res[,c("iter","residual","residualLag","bit")],title="AR(1) Residuals")
    names(tmp1)[2:3]=c("x","y")
    
    tmp2=cbind(res[,c("iter","qqx","qqy","bit")],title="Normal Q-Q Plot")
    names(tmp2)[2:3]=c("x","y")
      
    tmp3=cbind(res[,c("iter","year","residual","bit")],title="Residuals by Year")
    names(tmp3)[2:3]=c("x","y")
    
    tmp4=cbind(res[,c("iter","year","residual","bit")],title="Residuals by Z")
    names(tmp4)[2:3]=c("x","y")
    
    tmp5=cbind(res[,c("iter","yHat","residual","bit")],title="Residuals by Length")
    names(tmp5)[2:3]=c("x","y")
    
    tmp6=cbind(res[,c("iter","year","z","bit")],title="Z by Year")
    names(tmp6)[2:3]=c("x","y")
    
    tmp=rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)
    
    p.=ggplot(tmp)+geom_point(aes(x,y,colour=factor(bit)))+facet_wrap(~title,scale="free")+
        geom_smooth(aes(x,y),se=FALSE)
    
    print(p.)
    
    invisible(p.)
    })



