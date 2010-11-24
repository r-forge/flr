# xsa_example - «Short one line description»
# xsa_example

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 24 Nov 2010 05:11:03 PM CET IM:

library(FLH)
library(FLXSA)

# generating a population at equilibrium based on: 
# 
# Linf=100cm, 25 years max lifespan
res <- genBRP(age=1:25, Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
    a1=1, sL=.5, sR=150, mat95=3, s=0.75, v=1e3)

# coerce FLBRP into an FLStock
stk <- as(res, 'FLStock')
name(stk) <- 'stock'
 
## add iters so we can perform a Monte Carlo simulation
stk <- propagate(stk, iter=100)
 
# project for increasing F: from fmsy to 2*fmsy in 99 years
ctrl <- fwdControl(data.frame(year=2:100, quantity="f"),
  val=c(fbar(res)[,1]) * seq(1,2,length.out=99))

# generate normally distributed residuals of the SR model to introduce
# variability in the stock object
resid <- FLQuant(exp(rnorm(99*100,0,.5)), dimnames=list(age=0,year=2:99,iter=1:100))

# project under the F scenario and with SR residuals
stk <- fwd(stk, ctrl=ctrl, sr=list(model=model(res), params=params(res)),
    sr.residuals=resid)

# create an index of abundance from CPUE, plusgroup set at age=10
cpue <- as(setPlusGroup(stk,10), "FLIndex")

# select years up to 99, as catch/F has not been predicted for year=100
cpue <- window(cpue,end=99)
 
# make it be a 1st Quarter Survey
range(cpue, c("startf","endf")) <- c(0, 0.25)
 
summary(cpue)
 
# add measurement and/or process error to CPUE: lnorm(log(mean)=0, log(sd)=0,3)
index(cpue) <- index(cpue)[] * rlnorm(prod(dim(index(cpue)[])), 0, 0.3)
# exp(rlnorm(100, log(index(cpue)), 0.3))


## Create a stock data set
xsastk <- setPlusGroup(stk,10)
xsastk <- window(xsastk,end=99)
 
## M?
m(xsastk)[]<-0.2
 
## Nothing up my sleeve
stock.n(xsastk)[] <- NA
harvest(xsastk)[] <- NA
 
## Perform assessment
xsa.ctrl <- FLXSA.control(maxit=50, qage=6, tspower=0)
xsastk <- xsastk + FLXSA(xsastk,cpue,xsa.ctrl, diag.flag=FALSE)
 
## Compare OM & MP
plot(FLStocks(MP=xsastk, OM=window(stk,end=99)))

#Biological
omBiol<-as(ple4,"FLBiol")
 
#Fleet
omFleet<-as(ple4,"FLFleet")


