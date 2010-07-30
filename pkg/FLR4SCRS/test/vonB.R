## length-at-age
vonB(0:20,FLPar(Linf=238.6,K=0.185,t0=-1.404))

#### Von Bertalanffy growth parameters
vB  <-FLPar(Linf=238.6,K=0.185,t0=-1.404)

vonB(0:20,vB)

## sexual dimorphism
vB<-FLPar(c(238.6,0.185,-1.404),c(3,2,1),dimnames=list(params=c("Linf","K","t0"),unit=c("male","female"),iter=1))
vB["Linf","female"]<-vB["Linf","male"]*1.25

vonB(FLQuant(0:20,dimnames=list(ages=0:20,unit=c("male","female"))),vB)

ggplot(grw[!is.na(grw$data),])+geom_line(aes(age,data,group=paste(unit,season),col=paste(unit,season)))


## seasonal growth
ages<-FLQuant(rep(0:20,4)+rep(c(0,.1,.7,.9),each=21),dimnames=list(age=0:20,season=1:4))
t.<-vonB(ages,FLPar(Linf=238.6,K=0.185,t0=-1.404))

ggplot(t.)+geom_line(aes(age,data))

## growth stanzas
ages<-FLCohort(FLQuant(0:20,dimnames=list(age=0:20,year=1980:2010)))
ages[]<-0:20
t.<-vonB(ages,FLPar(Linf=238.6,K=0.185,t0=-1.404))

ggplot(t.)+geom_line(aes(age,data))

