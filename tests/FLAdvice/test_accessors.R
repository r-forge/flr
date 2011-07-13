# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLAdvice)

# accessors {{{
context("FLBRP accessors")

#
ob <- FLBRP()

# FLBRP(missing, missing)
test_that("FLBRP accessors work", {
    expect_that(landings.obs(ob), is_a("FLQuant"))
}) 

# }}}

