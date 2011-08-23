#### Jacknife
data(ple4)
data(ple4.index)
index(ple4.index)<-jacknife(index(ple4.index))
ple4XSA<-FLXSA(ple4,ple4.index,diag.flag=F)
plot(fbar(ple4))