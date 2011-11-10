#### Plots ##########################################################################################
# Quantile-comparison plots (J. Fox)

# last modified 30 September 2009 by J. Fox
# November 2009 by S. Weisberg -- changed to use showLabels for point identification
# 14 April 2010: set id.n = 0. J. Fox
# 1 June 2010: set reps=100 in qqPlot.lm. J. Fox
# 28 June 2010: fixed labeling bug S. Weisberg
# 11 March 2011: moved up ... argument. J. Fox

qqPlot.default <- function(x, distribution="norm", ..., ylab=deparse(substitute(x)),
  	xlab=paste(distribution, "quantiles"), main=NULL, las=par("las"),
		envelope=.95,  
		col=palette()[1], col.lines=palette()[2], lwd=2, pch=1, cex=par("cex"), 
		line=c("quartiles", "robust", "none"), 
		labels = if(!is.null(names(x))) names(x) else seq(along=x),
		id.method = "y", 
		id.n = if(id.method[1]=="identify") Inf else 0,
		id.cex=1, id.col=palette()[1], grid=TRUE, noShow=TRUE)
{
res=list()

	line <- match.arg(line)
	good <- !is.na(x)
	ord <- order(x[good])
	ord.x <- x[good][ord]
	ord.lab <- labels[good][ord]
	q.function <- eval(parse(text=paste("q", distribution, sep="")))
	d.function <- eval(parse(text=paste("d", distribution, sep="")))
	n <- length(ord.x)
	P <- ppoints(n)
	z <- q.function(P, ...)
	if (!noShow) plot(z, ord.x, type="n", xlab=xlab, ylab=ylab, main=main, las=las)
	if(grid & !noShow){
		grid(lty=1, equilogs=FALSE)
		box()}
	if (!noShow) points(z, ord.x, col=col, pch=pch, cex=cex)
	if (line == "quartiles" || line == "none"){
		Q.x <- quantile(ord.x, c(.25,.75))
		Q.z <- q.function(c(.25,.75), ...)
		b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
		a <- Q.x[1] - b*Q.z[1]
		if (!noShow) abline(a, b, col=col.lines, lwd=lwd)
		res$a=a
		res$b=b
	}
	if (line=="robust") {
		coef <- coef(rlm(ord.x ~ z))
		a <- coef[1]
		b <- coef[2]
		if (!noShow) abline(a, b)
		res$a=a
		res$b=b
	}
	conf <- if (envelope == FALSE) .95 else envelope
	zz <- qnorm(1 - (1 - conf)/2)
	SE <- (b/d.function(z, ...))*sqrt(P*(1 - P)/n)
	fit.value <- a + b*z
	upper <- fit.value + zz*SE
	lower <- fit.value - zz*SE
	res$z=z
        res$lower=lower
        res$upper=upper
	if (envelope != FALSE & !noShow) {
		lines(z, upper, lty=2, lwd=lwd, col=col.lines)
		lines(z, lower, lty=2, lwd=lwd, col=col.lines)
	}
	if (!noShow) showLabels(z, ord.x, labels=ord.lab,
		  	id.method = id.method, id.n = id.n, id.cex=id.cex, id.col=id.col)

invisible(res)}


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
                geom_line(aes(year,z,   group=bit+iter*10000),size=1.5) +
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
    ref1=cbind(x=range(tmp1$x,na.rm=TRUE),y=predict(lm(y~x,data=tmp1,na.action=na.omit),data.frame(x=range(tmp1$x,na.rm=TRUE))))
 
    qq=qqPlot.default(res$residual)
    
    tmp2=cbind(res[,c("iter","qqx","qqy","bit")],title="Normal Q-Q Plot")
    names(tmp2)[2:3]=c("x","y")
    ref2=data.frame("x"=c(range(tmp2$x,na.rm=TRUE)))
    ref2$y =ref2$x*qq$b + qq$a
    
    tmp3=cbind(res[,c("iter","year","residual","bit")],title="Residuals by Year")
    names(tmp3)[2:3]=c("x","y")
    ref3=data.frame(x=range(tmp3$x,na.rm=TRUE),y=c(0,0))
 
    tmp4=cbind(res[,c("iter","z","residual","bit")],title="Residuals by Z")
    names(tmp4)[2:3]=c("x","y")
    ref4=data.frame(x=range(tmp4$x,na.rm=TRUE),y=c(0,0))
    
    tmp5=cbind(res[,c("iter","yHat","residual","bit")],title="Residuals by Length Hat")
    names(tmp5)[2:3]=c("x","y")
    ref5=data.frame(x=range(tmp5$x,na.rm=TRUE),y=c(0,0))
 
    tmp6=cbind(res[,c("iter","year","z","bit")],title="Z by Year")
    names(tmp6)[2:3]=c("x","y")
    
    ttl=c("Z by Year","Normal Q-Q Plot","AR(1) Residuals","Residuals by Year","Residuals by Z","Residuals by Length Hat")
    tmp=transform(rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6), bit=factor(bit), title=factor(title,levels=ttl))
    ref=transform(cbind(rbind(ref1,ref2,ref3,ref4,ref5),title=rep(ac(unique(tmp$title)[-6]),each=2)), title=factor(title,levels=ttl))
 
    tmp=transform(tmp,ymin=pmin(y,0),ymax=pmax(y,0))
    tmp[ac(tmp$title) %in% ttl[1:2] ,c("ymin","ymax")]=NA
 
    p.=ggplot(tmp)+geom_point(aes(x,y,colour=bit),position = position_dodge(width = 0.010))+
        geom_smooth(aes(x,y),se=FALSE) +
        geom_path(aes(x,y),data=ref, col="red")   +
        geom_linerange(aes(x,ymin=ymin,ymax=ymax),position = position_dodge(width = 0.010))  +
        geom_line(aes(z,lower),data=data.frame(title="Normal Q-Q Plot",qq[3:4]),colour="red",lty=2)+
        geom_line(aes(z,upper),data=data.frame(title="Normal Q-Q Plot",qq[c(3,5)]),colour="red",lty=2)+
     facet_wrap(~title,scale="free")
    
    print(p.)
    
    invisible(p.)
    })
    
