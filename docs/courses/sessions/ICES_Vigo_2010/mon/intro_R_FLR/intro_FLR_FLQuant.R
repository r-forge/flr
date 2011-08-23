# intro_FLR - «Short one line description»
# intro_FLR

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Mon 29 Mar 2010 12:30:34 PM CEST IM:


# installation
# From FLR repository at http://flr-project.org
install.packages(repos="http://flr-project.org/R")

# loading FLCore to start with
library(FLCore)

# FLQuant
# Creating an empty one using the class creator
FLQuant()

# at heart FLQuant is an array
flq <- FLQuant()
is(flq)

# with some attributes
attributes(flq)

# like dimnames
dimnames(flq)

# dimensions
dim(flq)

# units
units(flq)

# and a name for the first dimension, 'quant'
quant(flq)

# some of which can be modified within limits
quant(flq) <- 'age'
summary(flq)

units(flq) <- 'kg'

# The creator method accepts 'smaller' classes,
# like vector
FLQuant(1:10)

# matrix
FLQuant(matrix(rlnorm(20), ncol=4, nrow=5))

# and array
FLQuant(array(rlnorm(160), dim=c(4,5,2,4)))

# The various attributes can also be set when building objects
# quant
FLQuant(1:10, quant='age')

# units
FLQuant(1:10, quant='age', units='t')

# dimnames
FLQuant(rlnorm(30), units='t', dimnames=list(age=1:3, year=2000:2009))

# or dim
FLQuant(rlnorm(50), dim=c(5,10))

# The sixth dimension, 'iter', is also treated differently
FLQuant(matrix(rlnorm(200), ncol=4, nrow=5), iter=10)
FLQuant(rlnorm(200), dim=c(4,5), iter=10)

flq <- FLQuant(rlnorm(2000), dimnames=list(age=0:9, year=1990:2009, iter=1:10),
  units='kg')

# some methods
# [ subsetting, always returns an FLQuant (unless drop=TRUE)
flq[1,]
flq[1,1,,,,drop=TRUE]

# apply, very useful to master
apply(flq, c(1,3:6), sum)
apply(flq, c(2:6), sum)

# sweep
flqmean <- apply(flq, 2, mean)
sweep(flq, 2, flqmean, "/")

# some convenience functions have been defined
quantSums(flq)
quantMeans(flq)
yearSums(flq)
iterMeans(flq)

# extract iters safely using iter()
iter(flq, 1)
iter(flq, 2)

flnoiter <- FLQuant(rlnorm(200), dimnames=list(age=0:9, year=1990:2009))
iter(flnoiter, 1)
iter(flnoiter, 2)

# trim & expand
trim(flq, year=1990:1995)
expand(flq, year=1990:2010)

# windows, trims along year dimension
window(flq, start=1990, end=1991)

# each class has a basic plot for simple inspection
plot(flq)

# but the useful thing to do is to build your own plots, lattice style,
# using a formula of type y ~ x | panels
# all data by year
xyplot(data ~ year, flq)
# separate panels by age
xyplot(data ~ year | age, flq)
# a trick to get numbers in panel strips
xyplot(data ~ year | as.factor(age), flq)

# using colours for ages in a single plot, just one iter
xyplot(data ~ year, group=age, iter(flq, 1), type='b', pch=19)

# or ploting variability using box-and-whisker
bwplot(data ~ year | as.factor(age), flq)

# getting help
?FLQuant

# inspecting code
showMethods('plot')
showMethods('plot', class='FLQuant')
getMethod('plot', c('FLQuant', 'missing'))

#  http://vimeo.com/1080428
