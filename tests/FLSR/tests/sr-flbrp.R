#### Create FLBRPs #############################################################
load("C:\\Stuff\\FLR\\Book\\data\\her4.RData")

ref  <-FLlst()
srr  <-FLlst()

      srr[["cushing"]]  <-as.FLSR(her4[["ices"]])
model(srr[["cushing"]]) <-cushing()
      srr[["cushing"]]  <-fmle(srr[["cushing"]] )
      ref[["cushing"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["cushing"]] ))
predict(srr[["cushing"]],ssb=ssb(ref[["cushing"]]))==rec(ref[["cushing"]])
plot( ref[["cushing"]])

      srr[["geomean"]]  <-as.FLSR(her4[["ices"]])
model(srr[["geomean"]]) <-geomean()
      srr[["geomean"]]  <-fmle(srr[["geomean"]] )
      ref[["geomean"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["geomean"]] ))
predict(srr[["geomean"]],ssb=ssb(ref[["geomean"]]))==rec(ref[["geomean"]])
plot( ref[["geomean"]])

      srr[["bevholt"]]  <-as.FLSR(her4[["ices"]])
model(srr[["bevholt"]]) <-bevholt()
      srr[["bevholt"]]  <-fmle(srr[["bevholt"]] )
      ref[["bevholt"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["bevholt"]] ))
predict(srr[["bevholt"]],ssb=ssb(ref[["bevholt"]]))==rec(ref[["bevholt"]])
plot( ref[["bevholt"]])

      srr[["ricker"]]  <-as.FLSR(her4[["ices"]])
model(srr[["ricker"]]) <-ricker()
      srr[["ricker"]]  <-fmle(srr[["ricker"]] )
      ref[["ricker"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["ricker"]] ))
predict(srr[["ricker"]],ssb=ssb(ref[["ricker"]]))==rec(ref[["ricker"]])
plot( ref[["ricker"]])

      srr[["shepherd"]]  <-as.FLSR(her4[["ices"]])
model(srr[["shepherd"]]) <-shepherd()
      srr[["shepherd"]]  <-fmle(srr[["shepherd"]] )
      ref[["shepherd"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["shepherd"]] ))
predict(srr[["shepherd"]],ssb=ssb(ref[["shepherd"]]))==rec(ref[["shepherd"]])
plot( ref[["shepherd"]])

      srr[["segreg"]]  <-as.FLSR(her4[["ices"]])
model(srr[["segreg"]]) <-segreg()
      srr[["segreg"]]  <-fmle(srr[["segreg"]] )
params(srr[["segreg"]])<-params(srr[["segreg"]])[c("a","b"),]
      ref[["segreg"]]  <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101),sr=srr[["segreg"]] ))
predict(srr[["segreg"]],ssb=ssb(ref[["segreg"]]))==rec(ref[["segreg"]])
plot( ref[["segreg"]])


ref[["per-rec"]] <-brp(FLBRP(her4[["ices"]],fbar=seq(0,1,length.out=101)))

##### Economics ################################################################
## Ogives
dnormal<-function(x,a,sL,sR){
  pow<-function(a,b) a^b

  func<-function(x,a,sL,sR){
    if (x < a) return(pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else       return(pow(2.0,-((x-a)/sR*(x-a)/sR)))}

  sapply(x,func,a,sL,sR)}

logistic<-function(x,a50,ato95){
  pow<-function(a,b) a^b

  func<-function(x,a50,ato95){
     if ((a50-x)/ato95 > 5)   return(0)
     if ((a50-x)/ato95 < -5)  return(1)

     return(1.0/(1.0+pow(19.0,(a50-x)/ato95)))}

  sapply(x,func,a50,ato95)}

prices    <-data.frame(rbind(cbind(Age=1:10,Price=dnormal( 1:10,3,10,20),Type="Peaking"),
                             cbind(age=1:10,Price=logistic(1:10,2,3),    Type="Increasing")))
prices$Age<-as.numeric(ac(prices$Age))

p    <- ggplot(prices,aes(x=Age, y=Price, group=Type))
p    <- p + geom_line(aes(colour=Type))
p

refIPrice<-ref[["per-rec"]]
refPPrice<-ref[["per-rec"]]

price(refIPrice)<-logistic(1:10,2,3)
price(refPPrice)<-dnormal( 1:10,3,1,5)

refIPrice<-brp(refIPrice)
refPPrice<-brp(refPPrice)

breakEven<-refIPrice
#### bug why not no recycling
refpts(breakEven)<-refpts(as.numeric(c(refpts(refIPrice)["fmax","revenue"]*2,rep(NA,7))),refpt=c("breakEven"))
computeRefpts(breakEven)[,"revenue"]

vcost(refIPrice)<-c(computeRefpts(breakEven)[,"revenue"]*0.20)
fcost(refIPrice)<-vcost(refIPrice)*4.0

vcost(refPPrice)<-vcost(refIPrice)
fcost(refPPrice)<-fcost(refIPrice)

refIPrice<-brp(refIPrice)
refPPrice<-brp(refPPrice)

price(refIPrice)<-price(refIPrice)/c(refpts(refIPrice)["mey","profit"])
price(refPPrice)<-price(refPPrice)/c(refpts(refPPrice)["mey","profit"])

refIPrice<-brp(refIPrice)
refPPrice<-brp(refPPrice)

plot(refPPrice)
plot(refIPrice)

plot( profit(refIPrice)~ssb(refIPrice),type="l")
lines(profit(refPPrice)~ssb(refPPrice))



ssbRng<-FLQuant(seq(0,max(ssb(srr[["geomean"]]))*1.1,length.out=101))

srrFits<-as.data.frame(FLQuants(
                                #"Mean"                =predict(srr[["geomean"]] ,ssb=ssbRng),
                                "Beverton & Holt"     =predict(srr[["bevholt"]] ,ssb=ssbRng),
                                "Ricker"              =predict(srr[["ricker"]]  ,ssb=ssbRng),
                                "Cushing"             =predict(srr[["cushing"]] ,ssb=ssbRng),
                                "Segmented Regression"=predict(srr[["segreg"]]  ,ssb=ssbRng),
                                "Shepherd"            =predict(srr[["shepherd"]],ssb=ssbRng)))

brpFits<-as.data.frame(FLQuants(
                               #"Mean"                =predict(srr[["geomean"]] ,ssb=ssbRng),
                                "Beverton & Holt"     =predict(srr[["bevholt"]] ,ssb=ssbRng),
                                "Ricker"              =predict(srr[["ricker"]]  ,ssb=ssbRng),
                                "Cushing"             =predict(srr[["cushing"]] ,ssb=ssbRng),
                                "Segmented Regression"=predict(srr[["segreg"]]  ,ssb=ssbRng),
                                "Shepherd"            =predict(srr[["shepherd"]],ssb=ssbRng)))

par(mfrow=c(1,3))
plot(   rec(ref[["geomean"]] )~ssb(ref[["geomean"]] ),type="l", xlab="SSB", ylab="Recruits")
lines(  rec(ref[["bevholt"]] )~ssb(ref[["bevholt"]] ),type="l")
lines(  rec(ref[["ricker"]]  )~ssb(ref[["ricker"]]  ),type="l")
lines(  rec(ref[["cushing"]] )~ssb(ref[["cushing"]] ),type="l")
lines(  rec(ref[["segreg"]]  )~ssb(ref[["segreg"]]  ),type="l")
lines(  rec(ref[["shepherd"]])~ssb(ref[["shepherd"]]),type="l")

plot(   yield(ref[["geomean"]] )~fbar(ref[["geomean"]] ),type="l", xlab="Yield", ylab="Fishing Mortality")
lines(  yield(ref[["bevholt"]] )~fbar(ref[["bevholt"]] ),type="l")
lines(  yield(ref[["ricker"]]  )~fbar(ref[["ricker"]]  ),type="l")
lines(  yield(ref[["cushing"]] )~fbar(ref[["cushing"]] ),type="l")
lines(  yield(ref[["segreg"]]  )~fbar(ref[["segreg"]]  ),type="l")
lines(  yield(ref[["shepherd"]])~fbar(ref[["shepherd"]]),type="l")

plot(   ssb(ref[["geomean"]] )~fbar(ref[["geomean"]] ),type="l", xlab="SSB", ylab="Fishing Mortality")
lines(  ssb(ref[["bevholt"]] )~fbar(ref[["bevholt"]] ),type="l")
lines(  ssb(ref[["ricker"]]  )~fbar(ref[["ricker"]]  ),type="l")
lines(  ssb(ref[["cushing"]] )~fbar(ref[["cushing"]] ),type="l")
lines(  ssb(ref[["segreg"]]  )~fbar(ref[["segreg"]]  ),type="l")
lines(  ssb(ref[["shepherd"]])~fbar(ref[["shepherd"]]),type="l")

plot(   yield(ref[["geomean"]] )~ssb(ref[["geomean"]] ),type="l", xlab="Yield", ylab="SSB")
lines(  yield(ref[["bevholt"]] )~ssb(ref[["bevholt"]] ),type="l")
lines(  yield(ref[["ricker"]]  )~ssb(ref[["ricker"]]  ),type="l")
lines(  yield(ref[["cushing"]] )~ssb(ref[["cushing"]] ),type="l")
lines(  yield(ref[["segreg"]]  )~ssb(ref[["segreg"]]  ),type="l")
lines(  yield(ref[["shepherd"]])~ssb(ref[["shepherd"]]),type="l")

##### Per recruit ##############################################################
par(mfrow=c(1,3))
plot(  ssb(ref[["per-rec"]])~fbar(ref[["per-rec"]]),lwd=1.5,type="l",xlab="Fishing Mortality",ylab="SSB per Recruit")
points(refpts(ref[["per-rec"]])[,"ssb"]~refpts(ref[["per-rec"]])[,"harvest"],  pch=c(16:19),col=1:4,cex=3)

plot(  yield(ref[["per-rec"]])~fbar(ref[["per-rec"]]),lwd=1.5,type="l",xlab="Fishing Mortality",ylab="Yield per Recruit")
points(refpts(ref[["per-rec"]])[,"yield"]~refpts(ref[["per-rec"]])[,"harvest"],pch=c(16:19),col=1:4,cex=3)

plot(  yield(ref[["per-rec"]])~ssb(ref[["per-rec"]]),lwd=1.5,type="l",xlab="SSB",ylab="Yield")
points(refpts(ref[["per-rec"]])[,"yield"]~refpts(ref[["per-rec"]])[,"ssb"],    pch=c(16:19),col=1:4,cex=3)

params= as.data.frame(FLQuants("Landings Sel"=landings.sel(ref[["per-rec"]]),
                               "Discards Sel"=discards.sel(ref[["per-rec"]]),
                               "Stock Wt"    =stock.wt(    ref[["per-rec"]]),
                               "M"           =m(           ref[["per-rec"]]),
                               "Maturity"    =mat(         ref[["per-rec"]])))

obs=    as.data.frame(FLQuants("F"       =fbar.obs(    ref[["per-rec"]]),
                               "Landings"=landings.obs(ref[["per-rec"]]),
                               "Discards"=discards.obs(ref[["per-rec"]]),
                               "Recruits"=rec.obs(     ref[["per-rec"]]),
                               "SSB"     =ssb.obs(     ref[["per-rec"]]),
                               "Stock"   =biomass.obs( ref[["per-rec"]])))

p    <- ggplot(params,aes(x=age, y=data, group=qname)) + facet_wrap(~qname,scale="free")
p    <- p + geom_line(aes(colour=qname))
p

p    <- ggplot(obs,   aes(x=year, y=data, group=qname)) + facet_wrap(~qname,scale="free")
p    <- p + geom_line(aes(colour=qname))
p


##### Biology ##################################################################
shp<-FLlst()
shp[["0"]] <-fmle(srr[["shepherd"]],fixed=list(c=0.0))
shp[["1"]] <-fmle(srr[["shepherd"]],fixed=list(c=1.0))
shp[["2"]] <-fmle(srr[["shepherd"]],fixed=list(c=2.0))
shp[["3"]] <-fmle(srr[["shepherd"]],fixed=list(c=3.0))
shp[["4"]] <-fmle(srr[["shepherd"]],fixed=list(c=4.0))
shp[["5"]] <-fmle(srr[["shepherd"]],fixed=list(c=5.0))
shp[["6"]] <-fmle(srr[["shepherd"]],fixed=list(c=6.0))

##### Fishery ##################################################################
discards.sel(ref[[1]])
landings.sel(ref[[1]])*logistic(1:10,2,3)

##### Uncertianty ##############################################################
#### Stock recruit
## Steepness
## Virgin biomass
## Shape

## Steepness * Shape

##### Monte Carlo ##############################################################
#### Stock recruit
#### Boot strap
#### Covar

    t. <-srr[["bevholt"]]
    t. <-fmle(t.)
ssb(t.)<-jacknife(ssb(srr[["bevholt"]]))
rec(t.)<-jacknife(rec(srr[["bevholt"]]))
    t. <-fmle(t.)

    t. <-srr[["cushing"]]
    t. <-fmle(t.)
ssb(t.)<-jacknife(ssb(srr[["cushing"]]))
rec(t.)<-jacknife(rec(srr[["cushing"]]))
    t. <-fmle(t.)

    t. <-srr[["ricker"]]
    t. <-fmle(t.)
ssb(t.)<-jacknife(ssb(srr[["ricker"]]))
rec(t.)<-jacknife(rec(srr[["ricker"]]))
    t. <-fmle(t.)

#### M

#### Selection Pattern

#### All

##### Mixed fisheries ##########################################################
#### ????

##### Advice ###################################################################
#### Kobe





