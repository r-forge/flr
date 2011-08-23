# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLCore) 
library(ggplotFL) 

runs <- paste("S", 0:12, sep="")

yft <- FLStocks(mlply(runs, function(x) readVPA2Box(paste(x, 'yft2008.ctl', sep="/"), m=0.8)))

names(yft) <- paste("Scenario", 0:12, sep=" ")
attributes(yft) <- attributes(yft)[1:4]
attr(yft, 'lock') <- TRUE
attr(yft, 'desc') <- paste('YFT ATL SA runs. Created on', date())

save(yft, file='yft.RData')
