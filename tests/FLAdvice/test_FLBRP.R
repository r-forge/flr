# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLAdvice)

# class(refpts) {{{
context("FLBRP()")

# FLBRP(missing, missing)
test_that("FLBRP(missing, missing) creates valid FLBRP objects", {
    expect_that(FLBRP(), is_a("FLBRP"))
    expect_that(validObject(FLBRP()), is_true())
}) 

# }}}

