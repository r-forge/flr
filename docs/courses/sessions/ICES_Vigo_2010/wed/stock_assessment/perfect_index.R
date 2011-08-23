#### creates a perfect index of abundance based on numbers-at-age
ple4Index<-as(ple4,"FLIndex")
ple4Index@range<-c(range(ple4Index),startf=0.0,endf=1.0)

#### runs XSA and comapares estimates with input values
ple4XSA<-FLXSA(ple4,ple4Index)

cf<-(stock.n(ple4)-stock.n(ple4XSA))/stock.n(ple4)
bubbles(age~year,data=cf)

#### Jacknife
data(ple4.index)
index(ple4.index)<-jacknife(index(ple4.index))
ple4XSA          <-FLXSA(ple4,ple4.index)