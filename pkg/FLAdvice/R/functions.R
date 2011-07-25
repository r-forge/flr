# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# shepherdCovB {{{

shepherdCovB<-function () {
    logl <- function(a, b, c, f, rec, ssb, covar) {
       hat<-a * ssb/(1 + (ssb/(b*(1+f*covar[[1]])))^c)
       loglAR1(log(rec), log(hat))}

    initial <- structure(function(rec, ssb) {
        c   <- 1.0
        f   <- 0.001
        x   <- ssb^c
        y   <- ssb/rec
        res <- coefficients(lm(c(y) ~ c(x)))
        a   <- max(1/res[1])
        b   <- max(b = 1/((res[2] * a)^(1/c)))
        return(FLPar(a=a, b=b, c=c, f=f))}, 

    lower = c(  0,   0,  1, -1), 
    upper = c(Inf, Inf, 10,  1))

    model <- rec ~ a * ssb/(1 + (ssb/(b*(1+f*covar[[1]])))^c)

    return(list(logl=logl, model=model, initial=initial))}
# }}}
