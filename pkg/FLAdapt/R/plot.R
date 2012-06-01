#### Plots ##########################################################################################
setGeneric('diags', function(object,...)
   standardGeneric('diags'))
setMethod("diags", signature(object="FLAdapt"),
  function(object) {
  
           
    rsdls=function(x){       
      x$residualLag <-c(x$residual[-1],NA)

      qq.  = qqnorm(c(x$residual),plot.it=FALSE)
      x$qqx= qq.$x
      x$qqy= qq.$y

      return(x)}
           
    res=ddply(object, .(cpue), rsdls)
    
    tmp1=cbind(res[,c("cpue","residual","residualLag")],title="AR(1) Residuals")
    names(tmp1)[2:3]=c("x","y")
    ref1=cbind(x=range(tmp1$x,na.rm=TRUE),y=predict(lm(y~x,data=tmp1,na.action=na.omit),data.frame(x=range(tmp1$x,na.rm=TRUE))))
 
    qq=qqPlot.default(res$residual)
    
    tmp2=cbind(res[,c("cpue","qqx","qqy")],title="Normal Q-Q Plot")
    names(tmp2)[2:3]=c("x","y")
    ref2=data.frame("x"=c(range(tmp2$x,na.rm=TRUE)))
    ref2$y =ref2$x*qq$b + qq$a
    
    tmp3=cbind(res[,c("cpue","year","residual")],title="Residuals by Year")
    names(tmp3)[2:3]=c("x","y")
    ref3=data.frame(x=range(tmp3$x,na.rm=TRUE),y=c(0,0))
 
    tmp4=cbind(res[,c("cpue","q","residual")],title="Residuals by Q")
    names(tmp4)[2:3]=c("x","y")
    ref4=data.frame(x=range(tmp4$x,na.rm=TRUE),y=c(0,0))
    
    tmp5=cbind(res[,c("cpue","hat","residual")],title="Residuals by Hat")
    names(tmp5)[2:3]=c("x","y")
    ref5=data.frame(x=range(tmp5$x,na.rm=TRUE),y=c(0,0))
 
    tmp6=cbind(res[,c("cpue","year","q")],title="Q by Year")
    names(tmp6)[2:3]=c("x","y")
    
    ttl=c("Z by Year","Normal Q-Q Plot","AR(1) Residuals","Residuals by Year","Residuals by Q","Residuals by Hat")
    tmp=transform(rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6), cpue=factor(cpue), title=factor(title,levels=ttl))
    ref=transform(cbind(rbind(ref1,ref2,ref3,ref4,ref5),title=rep(ac(unique(tmp$title)[-6]),each=2)), title=factor(title,levels=ttl))
 
    tmp=transform(tmp,ymin=pmin(y,0),ymax=pmax(y,0))
    tmp[ac(tmp$title) %in% ttl[1:2] ,c("ymin","ymax")]=NA
 
    p.=ggplot(tmp)+geom_point(aes(x,y,colour=cpue),position = position_dodge(width = 0.010))+
        geom_smooth(aes(x,y),se=FALSE) +
        geom_path(aes(x,y),data=ref, col="red")   +
        geom_linerange(aes(x,ymin=ymin,ymax=ymax),position = position_dodge(width = 0.010))  +
        geom_line(aes(z,lower),data=data.frame(title="Normal Q-Q Plot",qq[3:4]),colour="red",lty=2)+
        geom_line(aes(z,upper),data=data.frame(title="Normal Q-Q Plot",qq[c(3,5)]),colour="red",lty=2)+
     facet_wrap(~title,scale="free")
    
    print(p.)
    
    invisible(p.)
    })
    
