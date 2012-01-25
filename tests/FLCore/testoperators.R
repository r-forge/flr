# operators.R - DESC
# operators.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(testthat)
library(FLCore)


# foo {{{
foo <- function(x, y, operator="%*%") {
  #(
  test_that(paste(operator, "operator returns the right object"), {
          
    res <- do.call(operator, list(x, y))
    # returns FLQuant
    expect_that(res, is_a("FLQuant"))

    # dims of fq
    expect_that(dim(res), equals(dim(fq)))

    # dimnames
  
    # value
    expect_that(c(res),
      is_equivalent_to(do.call(gsub("%", "", operator), list(c(x), c(y)))))
  })
} # }}}

# context %% operators FLPar, FLQuant {{{
context("%% operators FLPar, FLQuant")

# 1. 
  fp <- FLPar(2, dimnames=list(params='a', year=1:3, iter=1))
  fq <- FLQuant(3, dim=c(3,3,1,1,1,1))

  # %*%
  foo(fp, fq, "%*%")
  # %+%
  foo(fp, fq, "%+%")
  # %-%
  foo(fp, fq, "%-%")
  # %/%
  foo(fp, fq, "%/%")

# 2. 
  fp <- FLPar(2, dimnames=list(params='a', year=1:3, iter=1))
  fq <- FLQuant(1:2, dim=c(3,3,1,1,1,1))

  # %*%
  foo(fp, fq, "%*%")
  # %+%
  foo(fp, fq, "%+%")
  # %-%
  foo(fp, fq, "%-%")
  # %/%
  foo(fp, fq, "%/%")

# }}}

  # context %% operators FLQuant, FLPar {{{
context("%% operators FLQuant, FLPar")

# 1. 
  fp <- FLPar(2, dimnames=list(params='a', year=1:3, iter=1))
  fq <- FLQuant(3, dim=c(3,3,1,1,1,1))

  # %*%
  foo(fq, fp, "%*%")
  # %+%
  foo(fq, fp, "%+%")
  # %-%
  foo(fq, fp, "%-%")
  # %/%
  foo(fq, fp, "%/%")
# }}}
