## Plotting
plotSR<-function(sr,maxSSB,cols=rainbow(9)){
  ssb<-FLQuant(seq(0,maxSSB,length.out=100))
  plot(rec(sr[[1]])~ssb(sr[[1]]),xlim=c(0,maxSSB),xlab="SSB",ylab="Recruits",pch=19)
  lines(lowess(ssb(sr[[1]]),rec(sr[[1]])))

  for (i in 1:length(sr))
    lines(predict(sr[[i]],ssb=ssb)~ssb,col=cols[i])
  }

plotSPR<-function(sr,ssb,maxSSB,cols=rainbow(9)){
  ssb<-FLQuant(seq(0,maxSSB,length.out=100))
  plot(rec(sr[[1]])/ssb(sr[[1]])~ssb(sr[[1]]),xlim=c(0,maxSSB),xlab="SSB",ylab="Recruits/SSB",pch=19)
  res<-lines(lowess(ssb(sr[[1]]),rec(sr[[1]])/ssb(sr[[1]])))

  for (i in 1:length(sr))
    lines(predict(sr[[i]],ssb=ssb)/ssb~ssb,col=cols[i])
  }

