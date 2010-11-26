# growthModels - «Short one line description»
# growthModels

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 25 Nov 2010 10:36:08 AM CET IM:

# vonB
# S = S_\infty (1 - e^{-\kappa (t - t_0)}
vonB <- function(Sinf, K, t0, a=1, x)
  return(Sinf * (1 - exp(-K * x - t0))^a)

# inversevonB
# \frac{t_0 - \log(1 - (\frac{x}{S_\infty})^{1/a})}{\kappa}
inversevonB <- function(Sinf, K, t0, a=1, x)
  return(t0 - log(1 - (x / Sinf)^(1 / a)) / K)

# gompertz
gompertz <- function(Sinf, b2, b3, x)
  return(Sinf * exp(-b1 * b3 ^ x))
