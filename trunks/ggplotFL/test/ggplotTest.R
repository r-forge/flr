#### FLQuant
data(ple4)

ggplot(stock.wt(ple4)) + geom_point(aes(age,data))

ggplot(stock.wt(ple4)) + geom_point( aes(age,data)) +
                         geom_smooth(aes(age,data))
                         
data(ple4sex)

ggplot(stock.wt(ple4sex)) + geom_point( aes(age,data,group=unit,col=unit)) +
                            geom_smooth(aes(age,data,group=unit,col=unit))

#### FLQuants
ggplot(FLQuants(all=stock.wt(ple4),male=stock.wt(ple4sex)[,,"male"],female=stock.wt(ple4sex)[,,"female"])) +
                         geom_point( aes(age,data,group=qname,col=qname)) +
                         geom_smooth(aes(age,data,group=qname,col=qname))

#### FLStock
plot(ple4)

#### FLStocks
plot(ple4)
