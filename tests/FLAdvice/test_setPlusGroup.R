# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLAdvice)

# setPlusGroup(FLBRP) {{{
context("setPlusGroup(FLBRP)")

#
data(ple4)
ple4sr <- fmle(as.FLSR(ple4, model='bevholt'))
ob <- FLBRP(ple4, sr=ple4sr)

# FLBRP(missing, missing)
test_that("setPlusGroup(FLBRP) works", {
    expect_that(setPlusGroup(ob, 20), is_a("FLBRP"))
}) 

# }}}

