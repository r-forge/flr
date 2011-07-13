# refpts.R - 

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLAdvice)

# refpts() {{{
context("refpts() creator")

# refpts(array, refpts, quantity, dimnames, iter)

# array, without dimnames, no dimnames
arr2d <- array(1:16, dim=c(2,8))
arr3d <- array(1:16, dim=c(2,8,1))

refpts(arr2d)
refpts(arr3d)

refpts(arr2d, refpt=c('msy', 'mey'))
refpts(arr2d, refpt=c('msy', 'mey'))[1,1,1]

# array, with dimnames
arr2dn <- array(1:16, dim=c(2,8), dimnames=list(refpt=c('msy', 'mey'),
    quantity=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit')))
arr3dn <- array(1:16, dim=c(2,8,1))

refpts(arr2dn)
refpts(arr3d)
refpts(arr2dn, refpt=c('msy', 'mey'))
refpts(arr2d, refpt=c('msy', 'mey'))[1,1,1]
# array, without dimnames, dimnames

# vector
vec <- 1:4
refpts(vec)
refpts(vec, refpt=c('msy', 'mey'))
refpts(vec, quantity=c('ssb'))

# matrix
mat <- matrix(1:16, ncol=8, nrow=2)

refpts(mat)

# missing
refpts()

refpts(refpt='msy')
# logical

# refpts


# }}}
