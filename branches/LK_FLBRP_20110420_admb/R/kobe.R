#### Quadrants
##### Kobe I ############################################################################################3
### provides the back drop on which to overlay data
kobe<-function(x,xlim=c(0,2),ylim=xlim){
       quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
                     data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
                     data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))

       ggplot(x)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
                   scale_fill_manual(values = c("yellow","green","red"), legend=FALSE) +
                   ylab(expression(F/F[MSY]))        +
                   xlab(expression(SSB/B[MSY]))      +
                   scale_y_continuous(limits=ylim)   +
                   scale_x_continuous(limits=xlim)}

kobeP<-function(biomass,harvest) {
            
            b =  pmin(as.integer(biomass),1)
            f =1-pmin(as.integer(harvest),1)
            p =f*b

            cbind(f=f,b=b,p=p,collapsed=(1-b)*(1-f))}

  
### Kobe Matrix ##################################################################################################
kobeM<-function(x, image  =list(levels=seq(0.0,1.0,0.05),
                                col    =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
                   contour=list(levels=c(.6,.7,1.0,.9),
                                col   =c("black")),
                   nIterp=501,xlab=NULL,ylab=NULL){

            x=subset(x, !is.na(x[,1]) & !is.na(x[,2]) & !is.na(x[,3]))

            ##### plot Kobe matrix
            t.<-akima::interp(x[,1],x[,2],x[,3],
                       xo=seq(min(x[,1]),   max(x[,1]), length=nIterp),
                       yo=seq(min(x[,2]),   max(x[,2]), length=nIterp),
                       duplicate="mean")

            if (!is.null(image)){      
	      ## Check ##################################################
              if (!(length(image$levels)-1 == length(image$col))) stop("image options differ")
            
              image(t., breaks=image$levels,col=image$col,
			xlab  =ifelse(is.null(xlab),names(x)[1],xlab),
			ylab  =ifelse(is.null(ylab),names(x)[2],ylab))}
	
            contour(t.,levels=contour$levels,col=contour$col,lwd=2,method="edge",labcex=1,add=!is.null(image$col))
       
            grid()
       
            invisible(tapply(x[,1],x[,c(2,3)],mean))}

##### plot Kobe lines
kobeL=function(x,col=list(levels=seq(0.0,1.0,0.05),
                          col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
               offSet  =1.5,
               cex.lgnd=0.75){
    iTAC =unique(x[,2])
    nTAC =length(iTAC)

    x[,1]<-as.numeric(as.character(x[,1]))
    cols =colorRampPalette(c("#2C2C2C" ,"#EEEEEE"))(nTAC)

    plot(x[x[,2]==iTAC[1],1],x[x[,2]==iTAC[1],3],
       type="n", ylab="Probability",xlab="Year",
       ylim=c(0,1), axes=F, #xlim=c(min(x[,1])-max(as.integer(diff(range(x[,1]))*0.1),offSet),max(x[,1])))
                            xlim=c(min(x[,1]),max(x[,1])++max(as.integer(diff(range(x[,1]))*0.1),offSet)))
     
    axis(side=1, at=seq(min(x[,1]),max(x[,1]),2))
    axis(side=2 ) ; box()
    if (!is.null(col)){      
      xRng<-rep(range(x[,1]),each=2)
      for (i in 1:length(col$col))
	polygon(xRng,c(col$levels[i],col$levels[i+1],col$levels[i+1],col$levels[i]),col=col$col[i],border=NA)}

    icol=0
    for( i in unique(x[,2])) {
       icol=icol+1
       lines(x[x[,2]==i,1], x[x[,2]==i,3],lwd=2,col=cols[icol],lty=1)}

    grid()
    legend("topright", as.character(iTAC), cex=cex.lgnd, lty=rep(1,nTAC), lwd=rep(2,16)    ,
    col= cols , bty="n")
    
    invisible(tapply(x[,3],x[,2:1],mean))}

kobeS<-function(x,cex=1.2){
    ops<-par(mfrow=c(2,2), mex=.5,mai=c( 0.5, 0.75 ,0.5, 0.1),cex=par()$cex)

    kobeM(x[,c(1:2,4)]);mtext(expression(plain(P) (B>=B[MSY])),                           line=0.5, cex=cex)
    kobeM(x[,c(1:2,5)]);mtext(expression(plain(P) (F<=F[MSY])),                           line=0.5, cex=cex)
    kobeM(x[,c(1:2,3)]);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=B[MSY])), line=0.5, cex=cex)
    kobeL(x[,c(1:2,3)]);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>= B[MSY])),line=0.5, cex=cex, side=3)
    
    #kobeM(xCont[,c("Year","P","TAC")]);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=B[MSY])), line=.5, cex=.8)
    
    par(mfrow=ops$mfrow,mex=ops$mex,mai=ops$mai,cex=ops$cex)}

