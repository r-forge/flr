# FLBRP - FLR class to calculate biological reference points

# Copyright 2010 Laurie Kell, ICCAT. Distributed under the GPL 2 or later
# $Id:  $


#### Load the package
library(FLBRP)

#### Doing something! ##########################################################
data(ple4)
pleBRP<-FLBRP(ple4)

## have a look
plot(pleBRP)

## reference points
refpts(pleBRP)

## Need to be calculated first
computeRefpts(pleBRP)

#### or can update updates refpts & equilibrium values
pleBRP<-brp(pleBRP)

plot(pleBRP)

#### Reference points ##########################################################
## You specify same by name e.g. crash
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("crash"))
computeRefpts(pleBRP)

## Create new ones  by specifying a value to be calculated
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("new one"))
refpts(pleBRP)["new one","ssb"]<-0
computeRefpts(pleBRP)

## FLim, FPA and target F
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("msy","Flim","Fpa","Ftarget"))
refpts(pleBRP)[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)
computeRefpts(pleBRP)

## BLim & BPA
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("msy","Blim","Bpa","Flim","Fpa","Ftarget"))
refpts(pleBRP)[c("Blim","Bpa"),          "ssb"]    <-c(160000,160000*1.4)
refpts(pleBRP)[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)
computeRefpts(pleBRP)

## For Biomass need to specify a stock recruitment relationship
params(pleBRP)<-mean(rec.obs(pleBRP))
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("msy","Blim","Bpa","Flim","Fpa","Ftarget"))
refpts(pleBRP)[c("Blim","Bpa"),          "ssb"]    <-c(160000,160000*1.4)
refpts(pleBRP)[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)
computeRefpts(pleBRP)

#### Economic Reference Points #################################################
# http://icesjms.oxfordjournals.org/cgi/content/abstract/65/6/1069

price(pleBRP)<-FLQuant(c(1.81,1.81,1.81,1.93,1.93,2.39,2.39,2.39,2.39,3.42),dimname=list(age=1:10))
fcost(pleBRP)<-1.40*2e5
vcost(pleBRP)<-0.47*2e5

plot(pleBRP)
plot(pleBRP,obs=T)

#### make it prettier by looking a smaller range of F
fbar(pleBRP)<-seq(0,1,length.out=101)
plot(pleBRP,obs=T)

#### Get rid of discarding #####################################################
discards.sel(pleBRP)<-0
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("msy","Blim","Bpa","Flim","Fpa","Ftarget"))
refpts(pleBRP)[c("Blim","Bpa"),          "ssb"]    <-c(160000,160000*1.4)
refpts(pleBRP)[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)
pleBRP<-brp(pleBRP)

plot(pleBRP,obs=T)

#### Mesh change ###############################################################
landings.sel(pleBRP)[ac(1:4)]<-0
refpts(pleBRP)<-refpts(as.numeric(NA),refpt=c("msy","Blim","Bpa","Flim","Fpa","Ftarget"))
refpts(pleBRP)[c("Blim","Bpa"),          "ssb"]    <-c(160000,160000*1.4)
refpts(pleBRP)[c("Flim","Fpa","Ftarget"),"harvest"]<-c(0.74,0.6,0.3)
pleBRP<-brp(pleBRP)

plot(pleBRP,obs=T)
