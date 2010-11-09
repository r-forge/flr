library(FLR4SCRS)

rho<-0.3;x<-rnorm(10001);for (i in 2:10001) x[i]<-x[i]+x[i-1]*rho
tst<-data.frame(rho=seq(-0.0,0.6,.01),mdply(seq(-0.0,0.6,.01), function(rho,x) loglAR1(x,rho), x=x[9000:10001]))

ggplot(tst)+geom_line(aes(rho,V1))

