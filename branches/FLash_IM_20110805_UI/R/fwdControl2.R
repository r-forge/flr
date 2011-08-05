# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# Global Variables' functions {{{
targetNames <- function()
  return(c("year", "min", "val", "max", "quantity", "season", "area", "unit",
      "spp", "fleet", "metier", "rel.year", "rel.season", "rel.area", "rel.unit"))
effortNames <- function()
  return(c("year", "min", "val", "max", "fleet", "metier", "rel.year", "rel.fleet",
      "rel.metier", "rel.bound"))
quantityNames <- function()
  return(c("ssb", "biomass", "catch", "landings", "discards", "f", "z", "f.landings",
      "f.discards", "effort", "costs", "revenue", "profit", "mnsz"))
# }}}

# validFwdControl {{{
validFwdControl <- function(object) {

  # rows in target and targetArray must match
  
  # rows in target and effort must match

  # rows in effort and effortArray must match

  # iters in targetArray and effortArray must match

  # 'quantity' must be in quantityNames

  return(TRUE)
}
# }}}

# fwdControl class {{{
setClass("fwdControl",
  representation(
    target = "data.frame",
    effort = "data.frame",
    targetArray = "array",
    effortArray = "array",
		block = "logical"), # specifies if mulitple rows are done together
  prototype = prototype(
    target = data.frame(),
    effort = data.frame(),
    targetArray = array(),
    effortArray = array(),
    block = FALSE),
	validity = validFwdControl
) # }}}

# fwdControl() {{{
fwdControl <- function(...) {

  args <- list(...)

  # convert each list element to a data.frame
  df <- lapply(args, as.data.frame)

  # merge elements
  df <- mergeRecursive(args, all=T)

  # sort by year
  df <- df[order(df$year), ]

  # create final data.frame
  dfnames <- targetNames()
  targetDF <- data.frame(matrix(NA, ncol=length(dfnames), nrow=nrow(df)))
  colnames(targetDF) <- dfnames

  # assign input dfs
  targetDF[,colnames(df)] <- df

}
# }}}

# fwdControl {{{
# }}}

# mergeRecursive {{{
mergeRecursive <- function(.list, ...) {
  if(length(.list)==1)
    return(.list[[1]])
	Recall(c(list(merge(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
} # }}}
