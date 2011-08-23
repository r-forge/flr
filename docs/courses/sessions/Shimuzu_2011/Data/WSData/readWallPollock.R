### Walleye Pollock
## CSV
wallPol<-read.csv("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\WalleyePollock.csv")
yrs    <-wallPol[1:29,1]
dat    <-wallPol[1:29,4:12]

is(yrs)
is(dat)
dat<-t(as.matrix(dat))
dat
is(dat)
caaPol<-FLQuant(dat,dimnames=list(age=2:10,year=yrs))
plot(caaPol)

## save it
save(caaPol,file="\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\caaPol.RData")