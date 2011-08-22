# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLCore) 
library(ggplotFL) 

runs <- paste("S", 0:12, sep="")

yft_mse <- FLStocks(mlply(runs, function(x) readVPA2Box(paste(x, 'yft2008.ctl', sep="/"), m=0.8)))

names(yft_mse) <- runs
attributes(yft_mse) <- attributes(yft_mse)[1:4]

save(yft_mse, file='yft.RData')
