# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(testthat)
library(FLCore)

# FLQuant() {{{
context("FLQuant()")

# FLQuant(missing)
test_that("FLQuant() returns the right FLQuant", {
  # class
  expect_that(FLQuant(), is_a("FLQuant"))
  # validObject
  expect_that(validObject(FLQuant()), is_true())
  # dim
  expect_that(dim(FLQuant()), equals(c(1,1,1,1,1,1)))
  # quant
  # units
  # dimnames
  # dim
  # iter
  # arg combinations
  # imcompatible combinations
}) 

# FLQuant(vector)

# FLQuant(matrix)

# FLQuant(array)

# }}}


