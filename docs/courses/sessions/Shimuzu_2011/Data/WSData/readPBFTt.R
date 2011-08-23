## pacific bft
pbft<-read.csv("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\ymfreq_PBFT.csv")
head(pbft)
pbft<-melt(pbft,
   id.vars=c("year","month"),
   measure.var=paste("X",30:100,sep=""))
head(pbft)
pbft$variable<-as.numeric(substr(pbft$variable,2,4))
names(pbft)[3]<-"age"

## save it
save(pbft,file="\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\pbft.RData")

ggplot(pbft)+geom_histogram(aes(value))

 