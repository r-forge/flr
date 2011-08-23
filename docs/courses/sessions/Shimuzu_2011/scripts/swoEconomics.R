##### chunk 1 ##################################################################
library(FLCore)   # use FLSR
library(FLBRP)    # refpts

load("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\RData\\swoSA.RData")

ls()
summary(swo)
plot(swo)

swoEcon<-brp(FLBRP(swo,fbar=seq(0,1,length.out=101)))
is(swoEcon)
summary(swoEcon)

landings(swoEcon)
landings.obs(swoEcon)

##### chunk 8 ##################################################################
##### Economics
#Fcost
fcost(swoEcon)
#vcost
vcost(swoEcon)
#price
price(swoEcon)

# revenue
revenue(swoEcon)
# cost
cost(swoEcon)
# profit
profit(swoEcon)

# MEY
refpts(swoEcon)


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

price(swoEcon)<-logistic(1:10,4,3)

swoEcon<-brp(swoEcon)

breakEven<-swoEcon
#### bug why not no recycling
refpts(breakEven)<-refpts(as.numeric(c(refpts(swoEcon)["fmax","revenue"]*2,rep(NA,7))),refpt=c("breakEven"))
computeRefpts(breakEven)[,"revenue"]

vcost(swoEcon)<-c(computeRefpts(breakEven)[,"revenue"]*0.20)
fcost(swoEcon)<-vcost(swoEcon)*4.0

swoEcon<-brp(swoEcon)

price(swoEcon)<-price(swoEcon)/c(refpts(swoEcon)["mey","profit"])

swoEcon<-brp(swoEcon)

plot(swoEcon)
################################################################################
