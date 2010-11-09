wKSM<-read.csv("C:/Stuff/ICCAT/SCRS/2010/BFT/analysis/VPA/West/wbft_ksm_2010.csv")

names(wKSM)<- c("Recruitment","Year","TAC","F","Bmsy","Probability")

### Kobe Matrix ##################################################################################################
kobeMatrix<-function(x){

    ##### plot Kobe matrix
    t.<-interp(x[,"Year"],x[,"TAC"],x[,"Probability"],
                          xo=seq(min(x[,"Year"]),  max(x[,"Year"]), length=101),
                          yo=seq(min(x[,"TAC"]),   max(x[,"TAC"]),  length=101))
    image(  t.,breaks=c(0,.5,.75,1),    col=c("red","yellow","green") ,ylab="TAC", xlab="Year")
    contour(t.,levels=c(.5,.75), add=T, col="grey",  lwd=2, method="edge", labcex=1)
    contour(t.,levels=c(.90,.60),add=T, col="grey2", lwd=2, method="edge", labcex=1)}
    
par(mfrow=c(2,2))
kobeMatrix(wKSM[wKSM$Recruitment=="BH",]);mtext("Beverton & Holt")
kobeMatrix(wKSM[wKSM$Recruitment=="2_line",]);mtext("2 Line")
kobeMatrix(wKSM[wKSM$Recruitment=="COMB",]):mtext("Combined")


library(akima)
library(ggplot2)

x<-melt(volcano)
x$value<-x$value-min(x$value)
x$value<-x$value/max(x$value)

x<-x[sample(1:dim(x)[1],100),]

##### plot
t. <- akima::interp(x[,"X1"], x[,"X2"], x[,"value"],
xo=seq(min(x[,"X1"]), max(x[,"X1"]), length=101),
yo=seq(min(x[,"X2"]), max(x[,"X2"]),length=101))
image( t.,breaks=c(0,.5,.70,1), col=c("red","yellow","green"))
contour(t.,levels=c(.5,.6,.70,.9), add=T,
col=c("black","grey","black","grey"), lwd=2, method="edge",
labcex=1)


srfc=akima::interp(x[,"X1"], x[,"X2"], x[,"value"])
srfc=data.frame(expand.grid(X1=srfc$x,X2=srfc$y),z=c(srfc$z),value=cut(c(srfc$z),breaks=c(0,.5,.75,1)))

ggplot(srfc) +
 geom_tile(aes(X1,X2,fill=value)) +
 geom_contour(aes(x=X1,y=X2,z=z),breaks=c(0,.5,.70,1),col=c("blue"))+
 geom_contour(aes(x=X1,y=X2,z=z),breaks=c(0.25,.45),col=c("green"))+
 scale_fill_manual(values=c("red","yellow","green"), name="Probability")

ggplot(srfc) +
 geom_tile(aes(X1,X2,fill=value)) +
 stat_contour(breaks=c(.60,.50,.20),aes(x=X1,y=X2,z=z, size=..level..)) +
 scale_fill_manual(values=c("red","yellow","green"), name="Probability")

