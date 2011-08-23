# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# $Id:  $

library(FLCore) 
library(ggplotFL) 

runs <- paste("S", 0:10, sep="")

albn <- FLStocks(mlply(runs, function(x)
  readMFCL(c(paste(x,"plot-09.par.rep",sep="/"),
      paste(x,"09.par", sep="/"))))
)

names(albn) <- paste("Scenario", 0:10, sep=" ")
attributes(albn) <- attributes(albn)[1:4]
attr(albn, 'lock') <- TRUE
attr(albn, 'desc') <- paste('N ATL ALB ATL MFCL SA runs. Created on', date(), ".",  R.Version()$version.string)

save(albn, file='albn.RData')
