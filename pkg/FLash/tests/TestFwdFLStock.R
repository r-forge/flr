#=====================================================================
#
# Date: 19/01/2009
# Version: 0.1-0
# Authors: Laurence Kell
#
# Short description: tests for fwd(FLStock)
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#
#=====================================================================

library(FLash)

data(ple4)

# Set range
ple4@range["plusgroup"]<-ple4@range["max"]
ple4@range[["minfbar"]]<-5
ple4@range[["maxfbar"]]<-10
ple4@landings.n<-ple4@catch.n*.75
ple4@discards.n<-ple4@catch.n-ple4@landings.n

# start test
setCon()
zz <- startTest("fwdFLStock.txt")
tagTest("fwd(FLStock) testing ...")


## Test fwd(FLStock ################################################################

##Target option
checkRun(
  Target <- fwdTarget(year=1998:2000,value=c(fbar(ple4)[1,as.character(1998:2000)]),quantity="f"))
  
##fwd()  
checkRun(
  res <- fwd.(ple4, Target, sr.model="geomean", sr.param= FLPar(array(25000,dim=c(1,3,1),dimnames=list(params="a",year=1998:2000,iter=1)))))
checkTrue(
  is(res,"FLStock"))

##compare output to input
checkEqual(
  fbar(ple4)[,as.character(1995:2000)],
  fbar(res)[ ,as.character(1995:2000)])

##checks all FLQuants are equal
for (i in names(getSlots("FLStock"))){
   if (class(slot(ple4,i))!="character"){
      slot(res, i)[is.na(slot(res, i))]<-0
      slot(ple4,i)[is.na(slot(ple4,i))]<-0}
      
   checkEqual(
     slot(ple4,i),slot(res,i))
     }


## SSB #########################################################################
# SSB targets are at end of year if harvest.spwn =0.0, otherwise in year
checkRun(
   Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="ssb"))

checkRun(
   res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(
   is(res,"FLStock"))

checkEqual(
   ssb(res)[1,as.character(2000:2001),drop=T],Target[-1,"value"])

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(
res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000)
)

checkEqual(
c(ssb(res)[1,as.character(2000:2001),drop=T]), c(ssb(res)[1,as.character(1995),drop=T])*Target[-1,"value"]
)

## SSB in year
checkRun(Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="ssb"))
ple4@harvest.spwn[]<-0.5

checkRun(res<-fwd(res, Target, sr.model="geomean", sr.param=25000))
checkEqual(ssb(res)[1,as.character(1998:2000),drop=T],Target[,"value"])

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))
checkEqual(ssb(res)[1,as.character(1998:2000),drop=T],c(ssb(res)[1,as.character(1995),drop=T])*Target[,"value"])

## Biomass #####################################################################
# Biomass targets are at end of year if harvest.spwn =0.0, otherwise in year
checkRun(Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="biomass"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(computeStock(res)[1,as.character(2000:2001),drop=T],Target[-1,"value"])

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(computeStock(res)[1,as.character(2000:2001),drop=T]),c(computeStock(res)[1,as.character(1995),drop=T])*Target[-1,"value"])

## Catch #######################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="catch"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

computeCatch(ple4)[,as.character(1995:2000)]
computeCatch(res)[ ,as.character(1995:2000)]
checkEqual(c(computeCatch(res)[1,as.character(2000:2001),drop=T]),c(Target[-1,"value"]))

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))


checkEqual(c(computeCatch(res)[1,as.character(2000:2001),drop=T]),c(computeCatch(res)[1,as.character(1995),drop=T])*Target[-1,"value"])

## Landings ####################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="catch"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(c(computeLandings(res)[1,as.character(2000:2001),drop=T]),Target[-1,"value"])

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(computeLandings(res)[1,as.character(2000:2001),drop=T]),c(computeLandings(res)[1,as.character(1995),drop=T])*Target[-1,"value"])

## Discards ####################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=c(200000,210000,220000),quantity="catch"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

computeDiscards(ple4)[,as.character(1995:2000)]
computeDiscards(res)[ ,as.character(1995:2000)]
checkEqual(c(computeDiscards(res)[1,as.character(2000:2001),drop=T]),Target[-1,"value"])

# check relative option by setting catch to 1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(computeDiscards(res)[1,as.character(2000:2001),drop=T]),c(computeDiscards(res)[1,as.character(1995),drop=T])*Target[-1,"value"])

#  F ###########################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=.4,quantity="f"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(c(fbar(res)[1,as.character(1998:2000),drop=T]),Target[,"value"])

# relative option, F  1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995

checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(fbar(res)[1,as.character(1998:2000),drop=T]),c(fbar(res)[1,as.character(1995),drop=T])*Target[,"value"])

## Z ###########################################################################
fbar.range<-as.character(ple4@range["minfbar"]:ple4@range["maxfbar"])
checkRun(Target<-fwdTarget(year=1998:2000,value=.75,quantity="z"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(fbar(res)[ ,as.character(1995:2000)] + apply(res@m[fbar.range,as.character(1995:2000)], 2, mean),Target[,"value"])

# relative option, Z  1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.25)
Target[,"rel"]  <-1995
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(fbar(res )[ ,as.character(1995:2000)] + apply(res@m[ fbar.range,as.character(1995:2000)], 2,mean)),
               c(fbar(ple4)[ ,as.character(1995:2000)] + apply(ple4@m[fbar.range,as.character(1995:2000)], 2,mean))*Target[,"value"])

## FLandings ###################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=.75,quantity="f.landings"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(c(apply((ple4@harvest*ple4@landings.n/ple4@catch.n)[fbar.range,as.character(1995:2000)],2,mean)) ,
               c(apply((res@harvest *res@landings.n /res@catch.n)[ fbar.range,as.character(1995:2000)],2,mean)))

# relative option, Z  1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkEqual(c(apply((res@harvest *res@landings.n  /res@catch.n)[ fbar.range,as.character(1998:2000)],2,mean)),
               c(mean((ple4@harvest*ple4@landings.n /ple4@catch.n)[fbar.range,as.character(1995)]))*Target[,"value"])

## FDiscards ###################################################################
checkRun(Target<-fwdTarget(year=1998:2000,value=.75,quantity="f.discards"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))

checkEqual(c(apply((ple4@harvest*ple4@discards.n/ple4@catch.n)[fbar.range,as.character(1995:2000)],2,mean)),
               c(apply((res@harvest *res@discards.n /res@catch.n)[ fbar.range,as.character(1995:2000)],2,mean)))

# relative option, Z  1, 0.5, 0.1 times that in 1995
Target[,"value"]<-c(1,.5,.1)
Target[,"rel"]  <-1995
res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000)

checkRun(checkEqual(c(apply((res@harvest *res@discards.n  /res@catch.n)[ fbar.range,as.character(1998:2000)],2,mean)) ,
                        c(mean((ple4@harvest*ple4@discards.n /ple4@catch.n)[fbar.range,as.character(1995)]))*Target[,"value"]))

## Effort ######################################################################
## Costs #######################################################################
## Revenue #####################################################################
## Profit ######################################################################
## MnSz ########################################################################

## Test for mixed targets ######################################################
checkRun(Target<-fwdTarget(year=1995:2000,value=c(.40,100000,5000,120000,220000,0.65),quantity=c("f", "ssb","catch","ssb","ssb","f")))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

## Test for multiple iters #####################################################
ple4<-propagate(ple4,iter=2,fill.iter=T)

checkRun(Target<-fwdTarget(year=1998:2000,value=.4,quantity="f"))
checkRun(res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000))

checkTrue(is(res,"FLStock"))
  
ple4@m[,,,,,2]<-.2
res<-fwd(ple4, Target, sr.model="geomean", sr.param=25000)
iter(fbar(res),1)
iter(fbar(res),2)
iter(ssb(res),1)
iter(ssb(res),2)

finishTest()
