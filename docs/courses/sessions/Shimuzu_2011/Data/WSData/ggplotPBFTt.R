pbft<-read.csv("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\ymfreq_PBFT.csv")
head(pbft)
t.<-melt(pbft,id.vars=c("year","month","catchN","sampleN"), measure.var=paste("X",30:100,sep=""))
head(t.)
t.$variable<-as.numeric(substr(t.$variable,2,4))
ggplot(t.)+geom_histogram(aes(variable,weight=value))+faccet_wrap(~year)
ggplot(t.)+geom_histogram(aes(variable,weight=value))+facet_grid(month~year)

stdz  <-function(x) ((x-mean(x))/sd(x))
minMax<-function(x) (x-min(x))/diff(range(x))


t.    <-ddply(t.,c("month","year"),transform,value=minMax(value))

ggplot(t.)+geom_histogram(aes(variable,weight=value))+facet_grid(month~year)
ggplot(t.)+geom_histogram(aes(variable,weight=value))+facet_grid(year~month)
