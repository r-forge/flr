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
res <- genBRP(age=1:25, fmsy=rep(1, 101), Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
    a1=1, sL=.5, sR=150, mat95=3, s=0.75, v=1e3)


