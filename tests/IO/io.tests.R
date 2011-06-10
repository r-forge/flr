# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

library(FLCore)

# ASPIC

# MED SWO

# MFCL

# ATL BET
betatl09 <- readMFCL(x=c('MFCL/ATL_BET/plot-09.par.rep', 'MFCL/ATL_BET/09.par'))

# VPA2Box

# ATL BFT
bftatl10 <- readVPA2Box(file='VPA2Box/ATL_BFT/bfte2010.c1')


# Pro2Box
library(FLAdvice)
t1 <-readPro2Box("Pro2Box/ATL_E_BFT/BENCH-1.OUT", type="ref")
