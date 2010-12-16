################################################################################
#                                                                              #
# Fitting Von Bertalanffy Growth Curve using ADMB-RE                           #
#                                                                              #
################################################################################

library(FLR4SCRS)
library(FLBioDym)

dirMy<-"C:/Stuff/Publications/inPrep/3stocks"
source("C:\\Stuff\\FLR\\pkg\\FLBioDym\\inst\\etc\\tests\\InputPolacheckData.R")
 

#### Generate data file
fnData<-function(dat,file,minCohort,maxCohort){
   admbOut(list("#",
                "# n; number of observations",dim(dat)[1],
                "# y; responses",             dat[,"data"],
                "# t; primary covariate",     dat[,"age"],
                "# ngroups",                  length(unique(pmax(minCohort,pmin(maxCohort,dat[,"cohort"])))),
                "# group",                    pmax(minCohort,pmin(maxCohort,dat[,"cohort"]))-min(pmax(minCohort,dat[,"cohort"]))+1),
           file)}

dat     <-as.data.frame(FLCohort(stock.wt(ple4[[2]][,,"female"])))[,c("age","cohort","data")]
dat     <-dat[!is.na(dat$data),]
fnData(dat,"C:/Stuff/FLR/workinprogress/rndWlk/logistic/vonB.dat",1957,2000)
checkDat<-admbIn("C:/Stuff/FLR/WorkInProgress/rndWlk/logistic/vonB.dat")

#### Run admb
setwd("C:/Stuff/FLR/WorkInProgress/rndWlk/logistic")
shell("vonB")
results <-admbIn("C:/Stuff/FLR/WorkInProgress/rndWlk/logistic/vonB.par")
