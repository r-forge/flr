### Jack Makerel

## Read in data
jackMak<-read.csv("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\jack.csv")
jackMak<-read.csv('/home/rob/Dropbox/FLRbook/Courses/Shimuzu/Server/Data/WSData/jack.csv')

caa<-FLQuant(unlist(c(jackMak[3:6,  -1])),dimnames=list(age=0:3,  year= substr(dimnames(jackMak)[[2]][-1],2,5)))
waa<-FLQuant(unlist(c(jackMak[11:14,-1])),dimnames=list(age=0:3,  year= substr(dimnames(jackMak)[[2]][-1],2,5)))
mat<-FLQuant(unlist(c(jackMak[15:18,-1])),dimnames=list(age=0:3,  year= substr(dimnames(jackMak)[[2]][-1],2,5)))
ctl<-FLQuant(unlist(c(jackMak[1,  -1])),dimnames=list(age="all",year= substr(dimnames(jackMak)[[2]][-1],2,5)))
maa<-FLQuant(unlist(c(jackMak[19, -1])),dimnames=list(age=0:3,  year= substr(dimnames(jackMak)[[2]][-1],2,5)))
zero<-maa
zero[]<-0
cpue<-FLQuant(unlist(c(jackMak[7:10,-1])),dimnames=list(age=0:3,  year= substr(dimnames(jackMak)[[2]][-1],2,5)))

plot(caa,scale="free",type="b")
plot(ctl,scale="free",type="b")

  ## Create FLStock Object
  jackMak<-FLStock(stock.wt    =waa,
                   catch.n     =caa,
                   catch.wt    =waa,
                   landings.n  =caa,
                   landings.wt =waa,
                   discards.n  =zero,
                   discards.wt =zero,
                   m           =maa,
                   mat         =mat)

  units(harvest(jackMak))<-"f"

  catch(   jackMak)<-computeCatch(   jackMak,"all")
  landings(jackMak)<-computeLandings(jackMak)
  discards(jackMak)<-computeDiscards(jackMak)

  harvest.spwn(jackMak)[]<-0.3
  m.spwn(      jackMak)[]<-0.3
  plot(jackMak)

## Create FLIndex Object
jackMakIdx<-FLIndex(index=cpue,name="Jack Makerel")
jackMakIdx<-window(jackMakIdx,start=2003)

range(jackMakIdx,c("startf","endf"))<-c(0,1)

## save data
#save(jackMak,jackMakIdx,file="\\\\192.168.100.101\\flr_2011\\FLR\\Data\\WSData\\jackMak.RData")

## fit XSA
xsa.control <- FLXSA.control(maxit=30, fse=1, qage=2, shk.ages=2, tspower=0)
jackMakXSA1  <- FLXSA(jackMak, FLIndices(jackMakIdx), xsa.control)

pfun <- function(x,y,...){
  panel.xyplot(x,y,...)
  panel.loess(x,y,...)
  panel.abline(h=0, lty=2, col='grey')
}
xyplot(data~year|age, data=index.res(jackMakXSA), type='p', panel=pfun)

xsa.control <- FLXSA.control(maxit=30, fse=1, qage=2, rage=-1, shk.ages=2, tspower=0)
jackMakXSA2  <- FLXSA(jackMak, FLIndices(jackMakIdx), xsa.control)

xyplot(data~year|age, data=index.res(jackMakXSA), type='p', panel=pfun)



## run assessment
jackMak          <-jackMak+FLXSA(jackMak,FLIndices(jackMakIdx))
index(jackMakIdx)<-jacknife(index(jackMakIdx))
jackMakJK        <-jackMak+FLXSA(jackMak,FLIndices(jackMakIdx))
plot(jackMak)
stock.n(jackMak)[1]

## Fit Stock Recruitment Relationship
jackMakSR<-as.FLSR(jackMak)
model(jackMakSR)<-bevholt()
jackMakSR<-fmle(jackMakSR)
plot(jackMakSR)
profile(jackMakSR)

#### jackknife
jackMakSRJK<-jackMakSR
ssb(jackMakSRJK)<-jacknife(ssb(jackMakSR))
jackMakSRJK<-fmle(jackMakSRJK)

plot(jackMakSRJK)
plot(params(jackMakSRJK))

## Estimate Reference points
jackMakBRP<-brp(FLBRP(jackMak,jackMakSR))
plot(jackMakBRP,obs=T)
kobe(jackMak)

jackMak20<-stf(jackMak,20)

Fmsy<-refpts(jackMakBRP)["msy","harvest",drop=T]
#### bug
ctrl     <-fwdControl(data.frame(year    =2009:2029,
                                 val     =Fmsy*.75,
                                 quantity="f"))


jackMak20<-fwd(jackMak20,ctrl=ctrl,sr=jackMakSR)
plot(jackMak20)

ssb.obs(jackMakBRP) <-ssb(jackMak20)
fbar.obs(jackMakBRP)<-fbar(jackMak20)

kobe(jackMakBRP)
