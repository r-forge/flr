# intro_FLR_FLStock - «Short one line description»
# intro_FLR_FLStock

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Fri 02 Apr 2010 12:41:13 PM CEST IM:

library(FLCore)

# loading example FLStock object, NS plaice
data(ple4)

# inspection
summary(ple4)

plot(ple4)

dims(ple4)

# range holds dimensions info, also plusgroup and fbar range
range(ple4)

# accessors defined for every slot
catch(ple4)
catch.n(ple4)[,'1990']

# many FLQuant methods also available at this level
summary(propagate(ple4, 10))

summary(ple4[,'1990'])

summary(trim(ple4, year=1990:1999))

summary(expand(ple4, year=1957:2057))

# and a number of methods for usual computations

# rec = stock.n[rec.age=first.age,]
rec(ple4)

# SSB = stock.n * exp(-F * F.spwn - M * M.spwn) * stock.wt * mat
ssb(ple4)

# Fbar = mean(F between fbar ages)
fbar(ple4)

# fapex = max F per year
fapex(ple4)

# ssbpurec = SSB per unit recruit
ssbpurec(ple4)

# r = stock reproductive potential
r(ple4)

# survprob = survival probabilities by year or cohort
survprob(ple4)
survprob(ple4, by ='cohort')

# sp = surplus production (delta stock + catch)
sp(ple4)

# lattice plots work on FLStock objects, use 'slot' keyword
xyplot(data ~ year | slot, ple4)

# probably more useful to extract individual slots into an 'FLQuants' list, use 'qname'
xyplot(data ~ year | qname, FLQuants(rec=stock.n(ple4)[1,], ssb=ssb(ple4), 
  catch=catch(ple4)), type='b', pch=19, scales=list(relation='free'))

# Coercion methods allow transfornmation
as.FLSR(ple4)
