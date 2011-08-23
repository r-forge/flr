#### Examples of forward projections with limits using FLassh::fwd #############

#### constant catch with an upper F bound
ctrl<-fwdControl(data.frame(year    =rep(2009:2028,each=2),
                            val     =rep(c(40000,NA),20),
                			      max     =rep(c(NA, 0.7),20),
                            quantity=rep(c("catch","f"),20)))
albFC<-fwd(alb20,ctrl=ctrl,sr=albSR)
plot(albFC)
computeCatch(albFC)
fbar(albFC)


#### 2.5% annual F reduction until F= 50% of F0.1
ctrl<-fwdControl(data.frame(year    =rep(2009:2028,each=2),
                            rel.year=c(t(array(c(2008:2027,rep(NA,20)),c(20,2)))),
                            val     =rep(c(0.975,NA),20),
                            min     =rep(c(NA,F0.1*.5),20),
                            quantity=rep(c("catch","f"),20)))
albFC<-fwd(alb20,ctrl=ctrl,sr=albSR)
plot(albFC)

#### 10% SSB increase ##########################################################
ctrl<-fwdControl(data.frame(year    =2009:2028,
                            rel.year=2008:2027,
                            min     =1.10,
                            quantity="ssb"))
albSSB<-fwd(alb20,ctrl=ctrl,sr=albSR)
plot(window(albSSB,end=2027))

#### Precautionary HCR ##########################################################
hcrF<-function(iYr,SSB,Bpa,Blim,Fmin,Fmax){
    val <-max(min((Fmax-(Fmax-Fmin)*(Bpa-SSB)/(Bpa-Blim)),Fmax),Fmin)
    trgt<-fwdControl(data.frame(year=iYr+1,quantity="f",val))

    return(trgt)}
    
pleProj     <-stf(ple4,nyear=15)
pleSR       <-as.FLSR(ple4)
model(pleSR)<-bevholt()
pleSR       <-fmle(pleSR)

for (iYr in 2008+1:14){
   ctrl<-hcrF(iYr,SSB=ssb(pleProj)[,ac(iYr-1)],Blim=160000,Bpa=160000*1.4,Fmin=.05,Fmax=.3)
   
   pleProj<-fwd(pleProj,ctrl=ctrl,sr=pleSR)}
   
plot(pleProj)


## calculate the constant F that will recover stock to target SSB (i.e. SSB in refYr) by targetYear
rebuildingPlan<-function(stk,sr,now,targetYr,refYr){

  ## 1st set control object to relative F
  ctrl     <-fwdControl(data.frame(year=now:targetYr,val=.5,rel=refYr,quantity="f"))

  ## set target SSB
  ssbTarget<-ssb(stk)[,ac(refYr)]

      ## function to minimise, i.e. so SSB in targetYr==refYr
      func<-function(x,stk,ssbTarget,targetYr,ctrl,sr){
         ctrl@target[,"val"]    <-x
         ctrl@trgtArray[,"val",]<-x
     
         ssb.<-c(ssb(fwd(stk,ctrl=ctrl,sr=sr))[,ac(targetYr)])
     
         return((ssb.-ssbTarget)^2)}

  ## do the minimisation using
  xmin<-optimise(func,c(0.1,1.0),tol=0.0000001,stk=stk,ssbTarget=ssbTarget,targetYr=targetYr,ctrl=ctrl,sr=sr)

  ## OK you´ve got the solution now use it
  ctrl<-fwdControl(data.frame(year=now:targetYr,val=xmin$minimum,rel=refYr,quantity="f"))
  res <-fwd(stk,ctrl=ctrl,sr=sr)

  return(res)}
  
#### run example
data(ple4)
 
# Set up the stock for the next 6 years
ple4 <-stf(window(ple4,end=2008),6)
 
# Set a constant recruitment based on the geometric mean of last 10 years
mnRec<-FLPar(exp(mean(log(rec(ple4)[,ac(1992:2001)]))))
 

ple4<-rebuildingPlan(ple4,sr=list(model="mean",params=mnRec),now=2009,targetYr=2014,refYr=1990)

plot(ple4)
