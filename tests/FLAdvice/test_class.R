# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLAdvice)

# class(refpts) {{{
context("refpts class")

# new('refpts')
test_that("refpts objects can be created and are valid", {
    # class
    expect_that(new("refpts"), is_a("refpts"))
    expect_that(new("refpts"), is_a("FLPar"))
    # validObject
    expect_that(validObject(new("refpts")), is_true())
}) 

# }}}

# class(FLBRP) {{{
context("FLBRP class")

# new('FLBRP')
test_that("FLBRP objects can be created and are valid", {
    # class
    expect_that(new("FLBRP"), is_a("FLBRP"))
    expect_that(new("FLBRP"), is_a("FLComp"))
    # validObject
    expect_that(validObject(new("FLBRP")), is_true())
}) 

# }}}
