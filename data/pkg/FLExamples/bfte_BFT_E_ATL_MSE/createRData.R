# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLCore) 

bfte <- FLStocks(inf13=readVPA2Box("Bootstraps/Inflated/run13/bfte2010.c1",sep=""),
  inf15=readVPA2Box("Bootstraps/Inflated/run15/bfte2010.c1"),
  rep13=readVPA2Box("Bootstraps/Reported/run13/bfte2010.c1"),
  rep15=readVPA2Box("Bootstraps/Reported/run15/bfte2010.c1"))

attributes(bfte) <- attributes(bfte)[1:4]

attr(bfte, 'lock') <- TRUE
attr(bfte, 'desc') <- paste('E ATL BFT VPA2Box SA runs. Created on', date(), ".",  R.Version()$version.string)

save(bfte, file='bfte.RData')
