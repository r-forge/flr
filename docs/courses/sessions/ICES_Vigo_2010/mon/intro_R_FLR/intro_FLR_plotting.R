# intro_FLR_plotting - «Short one line description»
# intro_FLR_plotting

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Sun 04 Apr 2010 11:24:15 AM CEST IM:

library(FLCore)

# FLR plots
# - based on lattice
# - defaults to conditioning on all dimensions of length > 1
# - intends to show content, not being THE plot

data(ple4)
plot(catch.n(ple4))

data(ple4sex)
plot(catch.n(ple4sex))

plot(ple4)

data(nsher)
plot(nsher)

# lattice
xyplot(data ~ year | as.factor(age), data=catch.n(ple4sex), groups=unit, type='l',
	scale=list(relation='free'), main="PLE4 catch by sex")

# panel function
xyplot(data ~ year | as.factor(age), data=catch.n(ple4sex), groups=unit, type='l',
	scale=list(relation='free'), main="PLE4 catch by sex",
	panel=function(...)
	{
		panel.xyplot(...)
		panel.superpose(panel.groups = panel.loess, ...)
	}
)

xyplot(data ~ year | as.factor(age), data=catch.n(ple4sex), groups=unit,
	scale=list(relation='free'), main="PLE4 catch by sex",
	panel=function(...)
	{
		panel.xyplot(..., type='l')
		panel.xyplot(..., type='p', col='black', cex=0.5)
	}
)

# work is done through conversion to data.frame:
# - FLQuant: data is called 'data'
head(as.data.frame(catch.n(ple4sex)))

# - FLQuants: FLQuant names are 'qname'
head(as.data.frame(FLQuants(catch.n=catch.n(ple4sex), catch.wt=catch.wt(ple4sex))))

# - FLComp: slots is called 'slot'
head(as.data.frame(ple4))



# ggplot2
