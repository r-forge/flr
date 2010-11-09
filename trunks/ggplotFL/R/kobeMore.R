#### Quadrants
##### Kobe I ############################################################################################3
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

### Kobe Matrix ##################################################################################################
kobeM<-function(x, img   =list(breaks=seq(0.0,1.0,0.05),
                                    col   =c(colorRampPalette(c("red4","red"))(12),
                                             colorRampPalette(c("yellowgreen","darkgreen"))(8))),
                       cntr   =list(levels=c(.6,.7,.8,.9),col=c("black")),
                       nIterp=501,xlab=NULL,ylab=NULL){
     ##### plot Kobe matrix
     t.<-akima::interp(x[,1],x[,2],x[,3],
                xo=seq(min(x[,1]),  max(x[,1]), length=nIterp),
                yo=seq(min(x[,2]),  max(x[,2]), length=nIterp))

     image(t., breaks=img$breaks,col=img$col,
               xlab=ifelse(is.null(xlab),names(x)[1],xlab),
               ylab=ifelse(is.null(ylab),names(x)[2],ylab))

     contour(t.,levels=cntr$levels,add=T,col=cntr$col,lwd=2,method="edge",labcex=1)

     grid()

     invisible(tapply(x[,3],x[,2:1],mean))}

##### plot Kobe lines
kobeLines=function(x){
    iTAC =unique(x[,2])
    nTAC =length(iTAC)

    x[,1]<-as.numeric(as.character(x[,1]))
    cols =colorRampPalette(c("#2C2C2C" ,"#EEEEEE"))(nTAC)

    plot(x[x[,2]==iTAC[1],1],x[x[,2]==iTAC[1],3],
     type="n", ylab="",cex.lab=.8,xlab="",
     ylim=c(0,1), axes=F, xlim=c(min(x[,1])-max(as.integer(diff(range(x[,1]))*.10),3),max(x[,1])))
     
    axis(side=1, at=seq(min(x[,1]),max(x[,1])))
    axis(side=2 ) ; box()
    polygon( rep(range(x[,1]), each=2),c(0.0,0.5,0.5,0.0), col="red")
    polygon( rep(range(x[,1]), each=2),c(0.5,0.7,0.7,0.5), col="yellow")
    polygon( rep(range(x[,1]), each=2),c(0.7,1.0,1.0,0.7), col="green")

    icol=0
    for( i in unique(x[,2])) {
       icol=icol+1
       lines(x[x[,2]==i,1], x[x[,2]==i,3],lwd=2,col=cols[icol],lty=1)}

    grid()
    legend("topleft", as.character(iTAC), cex=.7, lty=rep(1,nTAC), lwd=rep(2,16)    ,
    col= rev(cols) , bty="n")
    
    invisible(tapply(x[,3],x[,2:1],mean))}

kobeSuite<-function(x){
  ops<-par()
  par(mfrow=c(2,2), mex=.5, mai=c( 0.4, 0.4 ,0.34, 0.15))
  kobeMatrix(x[,c("Year","TAC","F")]);           mtext(expression(plain(P) (F<=F[MSY])),                         line=.5, cex=.8)
  kobeMatrix(x[,c("Year","TAC","Bmsy")]);        mtext(expression(plain(P) (B>=B[MSY])),                         line=.5, cex=.8)
  kobeMatrix(x[,c("Year","TAC","Probability")]); mtext(expression(plain(P) (F<=F[MSY]) * plain(P)(SSB>=B[MSY])), line=.5, cex=.8)
  kobeLines( x[,c("Year","TAC","Probability")]); mtext(expression(plain(P) (F<=F[MSY]) * plain(P)(SSB>= B[MSY])),line=0.5,cex=.8,side=3)
  par(mfrow=ops$mfrow,mex=ops$mex,mai=ops$mai)}

