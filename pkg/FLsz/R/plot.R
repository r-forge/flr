#### Plots ##########################################################################################

setMethod('plot', signature(x='FLsz',y="missing"),
    function(x,y,...){

    jk=any(unlist(qapply(x, function(x)
            ifelse("jacknife" %in% names(attributes(x)), return(attributes(x)$jacknife), return(FALSE)))))
  
    if (!jk) if (!is.null(attributes(x)$jacknife)) jk=attributes(x)$jacknife

    if (jk) p.=plotJK(x,y,...) else
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


plotJK=  function(x,y,...){
    dat=merge(getHat(x),as.data.frame(ln2z(x),drop=T))
    
    p.=ggplot(subset(dat,iter>1))+
              geom_point( aes(year,data,colour=factor(year)))  +
              #expand_limits(y = 0) +
              theme_flr(size=12.5)   +
              scale_y_continuous(name="Z") +
              scale_x_continuous(name="Year") +
              geom_line(aes(year,z,   group=iter+bit*100000,colour=factor(iter)),size=1.5) +
              geom_smooth(aes(year,data,group=factor(iter),colour=factor(iter)),span=0.75,lty=2,size=0.5,se=FALSE) +
              geom_line(aes(year,z,group=bit),size=2,data=subset(dat,iter==1)) +
              geom_smooth(aes(year,data),span=0.75,lty=1,size=1.0,se=FALSE,colour="black",data=subset(dat,iter==1)) +
              opts(legend.position = "none")
}

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
           
    res=ddply(res, .(iter), rsdls)[,c("year","bit","iter","sd","z","y","yHat","residual","residualLag","qqx","qqy")]
    
    if (!plotIt) return(res)
    
    tmp1=cbind(res[,c("iter","residual","residualLag","bit")],title="AR(1) Residuals")
    names(tmp1)[2:3]=c("x","y")
    ref1=cbind(x=range(tmp1$x),y=predict(lm(y~x,data=tmp1),data.frame(x=range(tmp1$x))))
 
    tmp2=cbind(res[,c("iter","qqx","qqy","bit")],title="Normal Q-Q Plot")
    names(tmp2)[2:3]=c("x","y")
    ref2=cbind(x=range(tmp2$x),y=predict(lm(y~x,data=tmp2),data.frame(x=range(tmp2$x))))
# 
#     ## create the confidence intervals
#     c95 <- rep(0,dim(tmp2)[1])
#     c05 <- rep(0,dim(tmp2)[1])
# 
#     ## the jth order statistic from a
#     ## uniform(0,1) sample
#     ## has a beta(j,n-j+1) distribution
#     ## (Casella & Berger, 2002,
#     ## 2nd edition, pg 230, Duxbury)
# 
#     for(i in 1:dim(tmp2)[1]){
#       c95[i] <- qbeta(0.95,i,dim(tmp2)[1]-i+1)
#       c05[i] <- qbeta(0.05,i,dim(tmp2)[1]-i+1)
#       }
# 
#     ci=data.frame(x=c95,y=c05,title="Normal Q-Q Plot")
#     
    tmp3=cbind(res[,c("iter","year","residual","bit")],title="Residuals by Year")
    names(tmp3)[2:3]=c("x","y")
    ref3=data.frame(x=range(tmp3$x),y=c(0,0))
 
    tmp4=cbind(res[,c("iter","z","residual","bit")],title="Residuals by Z")
    names(tmp4)[2:3]=c("x","y")
    ref4=data.frame(x=range(tmp4$x),y=c(0,0))
    
    tmp5=cbind(res[,c("iter","yHat","residual","bit")],title="Residuals by Length Hat")
    names(tmp5)[2:3]=c("x","y")
    ref5=data.frame(x=range(tmp5$x),y=c(0,0))
 
    tmp6=cbind(res[,c("iter","year","z","bit")],title="Z by Year")
    names(tmp6)[2:3]=c("x","y")
    
    ttl=c("Z by Year","Normal Q-Q Plot","AR(1) Residuals","Residuals by Year","Residuals by Z","Residuals by Length Hat")
    tmp=transform(rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6), bit=factor(bit), title=factor(title,levels=ttl))
    ref=transform(cbind(rbind(ref1,ref2,ref3,ref4,ref5),title=rep(ac(unique(tmp$title)[-6]),each=2)), title=factor(title,levels=ttl))
 
    tmp=transform(tmp,ymin=pmin(y,0),ymax=pmax(y,0))
    tmp[ac(tmp$title) %in% ttl[1:2] ,c("ymin","ymax")]=NA
    p.=ggplot(tmp)+geom_point(aes(x,y,colour=bit))+
        geom_smooth(aes(x,y),se=FALSE) +
        geom_path(aes(x,y),data=ref, col="red")   +
        geom_linerange(aes(x,ymin=ymin,ymax=ymax))  +
    #    geom_line(aes(x,y),data=ci)       +
        facet_wrap(~title,scale="free")
    
    print(p.)
    
    invisible(p.)
    })
    
