# FLBRP - FLR class to calculate biological reference points
# Copyright 2010 Laurie Kell, ICCAT. Distributed under the GPL 2 or later
# $Id:  $

#### Load the package
library(FLBRP)

#### Creating an object ########################################################

# The simplest way to create a new instance, i.e. object of a class, is from an
# existing object of a appropriate class, i.e. from an FLStock.

## Load the example North Sea Plaice FLStock object from FLCore
data(ple4)

## Create an FLBRP object
pleBRP<-FLBRP(ple4)

## Inspect the object
pleBRP

## summary is easier to get a quick overview
summary(pleBRP)

## Reference points
refpts(pleBRP)

## Equilibrium Fishing mortality
fbar(pleBRP)

#### “Bespoke” objects can be created using the creator ########################
## i.e. setting how averaging is done
pleBRP <-FLBRP(ple4, nyear=10)

pleBRP <-FLBRP(ple4, fbar=seq(0, 4, 0.04), nyears     =10,
                                           biol.nyears=10,
                                           fbar.nyears=3,
                                           sel.nyears =3,
                                           na.rm=TRUE, mean='arithmetic')


#### parameters correspond to the average or expected growth , maturity, natural mortality, selextrivity
stock.wt(pleBRP)
apply(stock.wt(ple4)[,ac(1999:2008)],1,mean)

## Biological parameters
m( pleBRP)
mat(pleBRP)

biol=FLQuants(sel=catch.sel(pleBRP),dsel=discards.sel(pleBRP),
              swt=stock.wt(pleBRP), cwt =catch.wt(pleBRP),
              mat= mat(pleBRP),     m   = m(pleBRP))

xyplot(data~age|qname,data=biol,type="l",scale="free")


#### Slots v. methods ##########################################################
# slots are data, methods are functions that do some calculations
# avoids inconsistencies, simplifies objects, easier to extend

# Selectivity
catch.sel(   pleBRP)
landings.sel(pleBRP)
discards.sel(pleBRP)

sel<-FLQuants(catch   =catch.sel(   pleBRP),
              discards=discards.sel(pleBRP),
              landings=landings.sel(pleBRP))

xyplot(data~age,data=sel,groups=qname,type="l",col=c("red","blue","brown"))

#### changing selectivity i.e. due to an increase in Mesh size
## doesn´t work since catch.sel is a method, i.e. landings.sel+discards.sel
catch.sel(   pleBRP)[ac(1:5)]<-catch.sel(   pleBRP)[ac(1:5)]*0.5

landings.sel(pleBRP)[ac(1:5)]<-landings.sel(pleBRP)[ac(1:5)]*0.5
discards.sel(pleBRP)[ac(1:5)]<-discards.sel(pleBRP)[ac(1:5)]*0.5

sel<-FLQuants(catch   =catch.sel(   pleBRP),
              discards=discards.sel(pleBRP),
              landings=landings.sel(pleBRP))

xyplot(data~age|qname,data=sel,type="l",col=c("red","blue","brown"))

## historic observations #######################################################
fbar.obs(    pleBRP)
yield.obs(   pleBRP)
landings.obs(pleBRP)
discards.obs(pleBRP)
rec.obs(     pleBRP)
ssb.obs(     pleBRP)
profit.obs(  pleBRP)

obs<-FLQuants(fbar    =fbar.obs(    pleBRP),
              yield   =yield.obs(   pleBRP),
              landings=landings.obs(pleBRP),
              discards=discards.obs(pleBRP),
              rec     =rec.obs(     pleBRP),
              ssb     =ssb.obs(     pleBRP),
              profit  =profit.obs(  pleBRP))
              
xyplot(data~year|qname,data=obs,type="l",scale="free")

#### Doing something! ##########################################################
