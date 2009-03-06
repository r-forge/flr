# refpts - «Short one line description»
# FLBRP/test/refpts.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Last Change: 06 Mar 2009 08:32
# $Id$

library(FLBRP)

#
data(ple4)

price <- FLQuant(c(0.0154,0.0633,0.1226,0.1768,0.2197,0.2514,0.2737,
          0.2890,0.2993,0.3062,0.3108,0.3138,0.3158,0.3171,0.3180),
          dimnames=dimnames(catch.n(ple4)))

sr <- as.FLSR(ple4, model='bevholt')
sr <- fmle(sr)

fbrp <- FLBRP(ple4, sr, price=price)

fbrp <- brp(fbrp)

summary(fbrp)

refpts(fbrp)

refpts(fbrp, refpt='fmax')
refpts(fbrp, quantity='yield')
refpts(fbrp, refpt='fmax', quantity='yield')

refpts(fbrp, refpt='fmax', quantity='harvest') <- 0.4
refpts(fbrp, 'fmax', 'harvest') <- 0.4
refpts(fbrp)

refpts(fbrp) <- computeRefpts(fbrp)

f0.1(fbrp)
fmax(fbrp)

