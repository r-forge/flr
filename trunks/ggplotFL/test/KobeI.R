#### Quadrants
kobeI <- qplot(xlab=expression(SSB/B[MSY]), ylab=expression(F/F[MSY])) +
           geom_polygon(aes(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf)), fill="yellow")  +
           geom_polygon(aes(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf)), fill="green")   +
           geom_polygon(aes(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1)), fill="red")     +
           scale_x_continuous(limits=c(0,2)) + scale_y_continuous(limits=c(0,2))

df<-data.frame(x=1+cumsum(rnorm(100,0,.1)),
               y=1+cumsum(rnorm(100,0,.1)),
               col=as.factor(1:100))

#### Points
p <- kobeI + geom_point(data=df,aes(x,y,col=col)) +
         scale_color_manual(values=gray(0:99 / 99),legend=FALSE)
#### Lines
p + geom_line(aes(1+cumsum(rnorm(20,0,.1)), 1+cumsum(rnorm(20,0,.1))),col="blue",lwd=2)


p <- qplot(xlab="Scenario", ylab=expression(F/F[MSY])) +
     geom_polygon(aes(x=c(0,0,5,5), y=c(0,1,1,0)), fill="white") +
     geom_polygon(aes(x=c(0,0,5,5), y=c(0,.75,.75,0)), fill="black")
p <- p + geom_point(aes((0:4)+0.5, runif(5,.6,.9), col="red", cex=4, pch=19)) +
     scale_x_continuous(breaks=(0:4)+0.5, labels=c("a","b","c","d","e"))


p <- p + geom_polygon(aes(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf)), fill="green")
p <- p + geom_polygon(aes(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1)), fill="red")

p <- p + scale_x_continuous(limits=c(0,2)) + scale_y_continuous(limits=c(0,2))
p <- p + geom_point(aes(rnorm(20,1,.1), rnorm(20,1,.1)))

p+geom_line(aes(1+cumsum(rnorm(20,0,.1)), 1+cumsum(rnorm(20,0,.1))),col="blue",lwd=2)



 values <- data.frame(
   id    = c("green","yellow","amber","red"),
   value = c("green","yellow","amber","red"))

 positions <- data.frame(
   id = rep(c("green","yellow","amber","red"), each = 4),
   x = c( 2, 1, 1, 2, 1, 0, 0, 1, 2, 1, 1, 2, 1, 0, 0, 1),
   y = c( 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2))

 # Currently we need to manually merge the two together
 datapoly <- merge(values, positions, by=c("id"))

 p <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(fill="yellow", col="grey")
 p <- p + geom_polygon(aes(fill=value, group=id))

################################################################################
 values <- data.frame(
   id    = c("green","yellow","amber","red"),
   value = c("green","yellow","amber","red"))

 positions <- data.frame(
   id = rep(c("green","yellow","amber","red"), each = 4),
   x = c( 2, 1, 1, 2, 1, 0, 0, 1, 2, 1, 1, 2, 1, 0, 0, 1),
   y = c( 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2))

 # Currently we need to manually merge the two together
 datapoly <- merge(values, positions, by=c("id"))

