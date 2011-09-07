# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

foo <- function(pkg, file="PACKAGES") {

  print(pkg)
  pkg <- unlist(strsplit(pkg, " "))

  for(p in pkg) {
      pinf <- packageDescription(p)

      cat(paste("Package:", pinf$Package), "\n",
        paste("Version:", pinf$Version), "\n",
        paste("Depends:", pinf$Depends), "\n",
        file=file, sep="", append=TRUE)
      cat("\n", file=file, append=TRUE)
  }

}
