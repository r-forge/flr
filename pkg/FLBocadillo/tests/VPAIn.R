yftDir<-"g:/PROJECTIONS_Resample"

yftBio    <-read.table(paste(yftDir,"BIO_t-1.OUT",sep="/"),header=F)
yftBioFish<-read.table(paste(yftDir,"BIO_f-1.OUT",sep="/"),header=F)
yftSSB    <-read.table(paste(yftDir,"SSBIO-1.OUT",sep="/"),header=F)
yftF      <-read.table(paste(yftDir,"Fapex-1.OUT",sep="/"),header=F)
yftCtch   <-read.table(paste(yftDir,"YIELD-1.OUT",sep="/"),header=F)
yftRpts   <-read.table(paste(yftDir,"BENCH-1.OUT",sep="/"),header=F,skip=1)

names(yftRpts)   <-c("iter",
                     "Fmsy","MSY","YRmsy","SRmsy","SPRmsy","SSBmsy",
                     "FMAX","YRMAX","SRMAX","SPRMAX","SSBMAX",
                     "F0.1","YR0.1","SR0.1","SPR0.1","SSB0.1",
                     "F20perc","YR20perc","SR20perc","SSB20perc",
                     "F30perc","YR30perc","SR30perc","SSB30perc",
                     "F40perc","YR40perc","SR40perc","SSB40perc",
                     "F90percYRmax","Y90percYRmax","90percYRmax","SR90perc","SSB90perc",
                     "F75percFmax","Y75percFmax","YR75percFmax","SR75percFmax","SSB75percFmax")

dmns=list(refpt   =c("msy","f0.1","fmax","spr.20","spr.30","spr.40","ypr.90","fmax.75"),
          quantity=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),
          iter    =unique(yftRpts[,"iter"]))

rfpts<-array(as.numeric(NA),unlist(lapply(dmns,length)),dmns)

rfpts["f0.1",   c("harvest",        "ssb"),]<-t(yftRpts[,c("F0.1",         "SSB0.1")])
rfpts["fmax",   c("harvest",        "ssb"),]<-t(yftRpts[,c("FMAX",         "SSBMAX")])
rfpts["msy",    c("harvest","yield","ssb"),]<-t(yftRpts[,c("Fmsy","MSY",   "SSBmsy")])
rfpts["spr.20", c("harvest",        "ssb"),]<-t(yftRpts[,c("F20perc",      "SSB20perc")])
rfpts["spr.30", c("harvest",        "ssb"),]<-t(yftRpts[,c("F30perc",      "SSB30perc")])
rfpts["spr.40", c("harvest",        "ssb"),]<-t(yftRpts[,c("F40perc",      "SSB40perc")])
rfpts["ypr.90", c("harvest",        "ssb"),]<-t(yftRpts[,c("F90percYRmax", "SSB90perc")])
rfpts["fmax.75",c("harvest",        "ssb"),]<-t(yftRpts[,c("F75percFmax",  "SSB75percFmax")])

rfpts<-refpts(rfpts)

getVPA2Boot<-function(x,minYr=1){
   func <-function(x,dt) {
      yrs <-(1:(dim(dt)[2]-2))+minYr-1
      dmns<-list(age="all",unit="unique",season="all",area="unique",iter=unique(dt[x,2]),year=yrs)

      res<-aperm(array(unlist(dt[x,-(1:2)]),unlist(lapply(dmns,length)),dmns),c(1,6,2,3,4,5))
      res<-FLQuant(res)
      }

   tapply(1:dim(x)[1], x[,1], func, dt=x)
   }
   
yftBio    <-getVPA2Boot(yftBio     ,1970)
yftBioFish<-getVPA2Boot(yftBioFish ,1970)
yftSSB    <-getVPA2Boot(yftSSB     ,1970)
yftF      <-getVPA2Boot(yftF       ,1970)
yftCtch   <-getVPA2Boot(yftCtch    ,1970)

save(yftBio, yftBioFish, yftSSB, yftF, yftCtch, yftRpts,file="c:/temp/yft.Rdata")

b<-lapply(yftSSB[seq(1,11,length.out=6)], function(x) sweep(x[,ac(38:47)],6,rfpts["msy","ssb",    ],"/"))
f<-lapply(yftF[  seq(1,11,length.out=6)], function(x) sweep(x[,ac(38:47)],6,rfpts["msy","harvest",],"/"))

#### Joint advice plots ########################################################
## Calculate probabilities
dmns<-list(val=c("joint","f","ssb"),TAC=seq(50,150,length.out=6),year=2007:2016,iter=0:500)
res <-array(NA,lapply(dmns,length),dimnames=dmns)
for (iScen in 1:6){
       res["ssb",  iScen,,]<-  qmin(floor(b[[iScen]]),1)[1,drop=T]
       res["f",    iScen,,]<-1-qmin(floor(f[[iScen]]),1)[1,drop=T]
       res["joint",iScen,,]<-res["f",iScen,,]*res["ssb",iScen,,]
       }
res<-apply(res["joint",,,],1:2,mean)

## Plot Joint probabilities
t.<-defactor(cbind(expand.grid(dimnames(res)[c("TAC","year")]),val=c(res)))
t.<-interp(t.[,2],t.[,1],t.[,3], yo=seq(min(t.[,1]), max(t.[,1]), length=500),
                                 xo=seq(min(t.[,2]), max(t.[,2]), length=500))
image(  t.,breaks=c(0,.5,.75,1), col=c("red","yellow","green"),ylab="TAC (1000 tonnes)", xlab="Year")
contour(t.,levels=c(.5,.75),add=T,  col="grey",  lwd=2)
contour(t.,levels=c(.90,.60),add=T, col="grey2", lwd=2)
savePlot(paste(myDir,"/figs/FlagJ.bmp",sep=""),type="bmp")
