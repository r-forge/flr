library(FLAssess)    # load the FLAssess package
library(ggplotFL)
library(FLash)

### Walleye Pollock ############################################################

##### Read in data & create an FLStock object
wpData<-read.csv("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\WalleyePollock.csv")
wpData<-read.csv("C:\\Stuff\\My Dropbox\\FLRbook\\Courses\\Shimuzu\\Server\\Data\\WSData\\WalleyePollock.csv")
yrs   <-wpData[1:29,1]
dat   <-t(as.matrix(wpData[1:29,4:12]))

cpue<-FLQuant(wpData[20:29,13],dimnames=list(age="all",year=yrs[19:29]))

caa   <-FLQuant(dat,dimnames=list(age=2:10,year=yrs))
waa   <-FLQuant(wpData[33,4:12],dimnames=list(age=2:10,year=yrs))
mat   <-FLQuant(wpData[34,4:12],dimnames=list(age=2:10,year=yrs))
maa   <-FLQuant(wpData[35,4:12],dimnames=list(age=2:10,year=yrs))
zero  <-FLQuant(0,dimnames=dimnames(maa))

wallPol<-FLStock(stock.wt    =waa,
                 catch.n     =caa,
                 catch.wt    =waa,
                 landings.n  =caa,
                 landings.wt =waa,
                 discards.n  =zero,
                 discards.wt =zero,
                 m           =maa,
                 mat         =mat,
                 harvest.spwn=zero,
                 m.spwn      =zero)

load('/home/rob/Dropbox/FLRbook/Courses/Shimuzu/wallPol.RData')

units(harvest(wallPol))<-"f"

catch(   wallPol)<-computeCatch(   wallPol,"all")
landings(wallPol)<-computeLandings(wallPol)
discards(wallPol)<-computeDiscards(wallPol)
plot(wallPol)


## nothing up my sleeve
harvest(wallPol)
stock.n(wallPol)

decade<-function(year){
            res<-year - (year %% 10)
            return(factor(res,seq(min(res),max(res),10)))}

biYr<-function(year){
            res<-year - (year %% 2)
            return(factor(res,seq(min(res),max(res),2)))}

ggplot(catch(wallPol))+geom_point(aes(year,data))+stat_smooth(aes(year,data))
p<-ggplot(catch.n(wallPol)[-9])
p<-p+geom_point(aes(age,log(data)))+stat_smooth(aes(age,log(data)))
p

p$data$decade<-decade(p$data$year)
p+facet_wrap(~decade)

# Make a control object
ctrl <- FLSepVPA.control(sep.nyr=29,sep.age=4,sep.sel=1.0)

# Run SepVPA
wallPolVPA <- SepVPA(wallPol, ctrl, fit.plusgroup=TRUE, ref.harvest=0.1, fratio=4)

rsdls<-log(catch.n(wallPolVPA)/catch.n(wallPol))

rsdls<-as.data.frame(rsdls[-9])[,c("data","age","year")]

p<-ggplot(rsdls[rsdls$data>0,])+geom_point(aes(year,age,size=data),col="black")+
   geom_point(aes(year,age,size=data),col="red",data=rsdls[rsdls$data<=0,])

# Use + to update the harvest and stock slots of the stock object
wPol <- wallPol + wallPolVPA
catch(   wPol)<-computeCatch(   wPol)
landings(wPol)<-computeLandings(wPol)
discards(wPol)<-computeDiscards(wPol)

plot(wPol)

computeStock(wPol)
ggplot(cpue/mean(cpue,na.rm=T))+geom_point(aes(year,data)) +
                  geom_point(aes(year,data),data=as.data.frame(ssb(trim(wPol,year=1998:2008))/mean(ssb(trim(wPol,year=1998:2008)))),col="red")


rsdl<-cpue/mean(cpue,na.rm=T)/(ssb(trim(wPol,year=1998:2008))/mean(ssb(trim(wPol,year=1998:2008))))
rsdl
loglAR1(rsdl[,-4])


# Estimate selection at age by dividing F by Fmax
xyplot(data~age, group=year, type="l", xlab="Age", ylab="Selection",
       data=FLQuant(sweep(harvest(stk.),2,apply(harvest(stk.),2,max),"/")))

#Time series of fishing mortality
xyplot(data~year, data=harvest(stk.)[ctrl@sep.age,ac(1990:2008)], type="l",xlab="Year", ylab="Fishing Mortality", ylim=c(0,1.25))

#Bubble plot of catch residuals
bubbles(age~year, data=catch.n(stk.)-catchHat, col=c("black","grey"))

save(wallPol,file="\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\wallPol.RData")
