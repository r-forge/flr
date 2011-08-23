# FLSR - «Short one line description»
# FLSR

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Tue 30 Mar 2010 09:54:27 AM CEST IM:

library(FLCore)

# Let's look at the FLSR class and methods

# Example dataset, North Sea Herring
data(nsher)

summary(nsher)

# what slots are in an FLSR object
getSlots('FLSR')

# set the model among the available ones
model(nsher) <- ricker

# look what ricker() contains

# a formula for the model
ricker()$model

# the loglikehood function
ricker()$logl
getMethod('loglAR1', c('FLQuant', 'FLQuant'))

# a function for initial values
ricker()$initial

# fmle is the fitting method using logl and R's optim
nsher <- fmle(nsher)

# basic plot, contains model fit an residuals
plot(nsher)

# for a 2-parameter model like this one, profiles over a range of values
# around MLE estimate
profile(nsher)

# fmle also allows individual parameters to be fixed
nsherFa <- fmle(nsher, fixed=list(a=130))

# methods exist for Akaike Information Criterion
AIC(nsher)
AIC(nsherFa)

# and Schwarz's Bayesian Information Criterion
BIC(nsher)
BIC(nsherFa)

# predict uses the formula and parameter values to get a predicted recruitment
predict(nsher)

# which can be called with a new input ssb
predict(nsher, ssb=ssb(nsher)*1.5)

# conversion between SV and AB models can be done on a fitted object,
# by providing a value of spr0
nsherSV <- sv(nsher, spr0=0.04)

summary(nsherSV)

# what models are available?
ricker()
bevholt()
shepherd()
cushing()
geomean()
segreg()
rickerSV()
bevholtSV()
shepherdSV()
bevholtAR1()
