# fwdTarget - «Short one line description»
# FLash/R/fwdTarget.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas
# Last Change: 11 Jun 2008 14:21
# $Id$

# target  {{{
#if (!isGeneric("fwdTarget"))
#	setGeneric("fwdTarget", function(object, ...)
#		standardGeneric("fwdTarget"))
#setMethod("fwdTarget", signature(object="list"),
#   function(object)
#   {
#     # turn into data.frame
#     if(class(object[[1]]) != 'list')
#       object <- list(object)
#     object <- lapply(object, function(x) data.frame(as.list(x)))
#     len <- sum(unlist(lapply(object, nrow)))
#     # ouput data.frame
#     res <- data.frame(year=rep(as.numeric(NA), len),
#       value=rep(as.numeric(NA), len), min=rep(as.numeric(NA), len),
#       max=rep(as.numeric(NA), len), quantity=rep(factor(0, levels=c("ssb","biomass",
#       "catch", "landings","discards","f","z","f.landings","f.discards","effort","costs",
#       "revenue", "profit")), len), rel=rep(as.numeric(NA), len),
#      spp   =rep(as.character('1'), len),
#      fleet =rep(as.character('1'), len),
#       metier=rep(as.character('1'), len),
#       stringsAsFactors=FALSE)
#     # load each target
#     elem <- c(0,unlist(lapply(object, nrow)))
#     for(i in seq(length(object)))
#       res[elem[i]+(1:nrow(object[[i]])), names(object[[i]])] <- object[[i]]
#
#     # check that first option for a year is a target
#     if (!all(!is.na(res[!duplicated(res[,"year"]),"value"])))
#        stop("Gotta specify a target for a year first")
#     # check that if max or min specified then no target & vice versa
#     if (any((!is.na(res[,"min"]) | !is.na(res[,"max"])) &
#       !is.na(res[,"value"])))
#       stop("Can't specify a target and a min or max values")
#     # check years are all given
#     if (any(is.na(res[,"year"])))
#       stop("All targets should have a year")
#
#     # delete spp/fleet/metier columns if not needed
#     if(all(res$spp == 1) && all(res$fleet == 1) && all(res$metier == 1) &&
#      !any(unlist(lapply(object, function(x) any(c('spp', 'metier', 'fleet') %in%
#      names(x))))))
#        res <- res[,!names(res) %in% c('spp', 'fleet', 'metier')]
#
#     return(res)
#   }
#)
#setMethod("fwdTarget", signature(object="missing"),
#   function(...)
#     fwdTarget(list(...))
#) # }}}


#******* Target Functions for C interface ************

checkTarget<-function(target)
    {
    # check that if max or min specified then no target & vice versa
    if (any((!is.na(target[,"min"]) | !is.na(target[,"max"])) & !is.na(target[,"val"]))) {
       warning("Can't specify a val and a min or max values")
       return(FALSE)}

    if (any((!is.na(target[,"min"]) & !is.na(target[,"max"])) & target[,"max"]<=target[,"min"])){
       warning("max less than than min value")
       return(FALSE)}

	# Should also check quantity

    return(TRUE)
    }
         
## create matrix for C++
#matrixTarget<-function(target){
#  res <- cbind(
#     as.double(target$year),
#     as.double(target$min),
#     as.double(target$val),
#     as.double(target$max),
#     as.double(target$quantity),
##     as.double(quantity[as.character(target$quantity)]),
#     as.double(target$spp),
#     as.double(target$fleet),
#     as.double(target$metier),
#     as.double(target$rel))
#     
#   return(matrix(res,dim(res)))}

matrixTarget <- function(target)
{
    #reorder columns for C code (???)
    target <- target[,c("year","min","val","max","quantity","spp","fleet","metier","rel")]
    for(i in names(target))
        target[,i] <- as.double(target[,i])
    return(matrix(unlist(target),dim(target)))
}

#matrixEffort<-function(effort){
#  res <- cbind(
#     as.double(effort$year),
#     as.double(effort$min),
#     as.double(effort$val),
#     as.double(effort$max),
#     as.double(effort$fleet),
#     as.double(effort$metier),
#     as.double(effort$rel.year),
#     as.double(effort$rel.fleet),
#     as.double(effort$rel.metier),
#     as.double(effort$rel.bound))
#   
#   return(matrix(res,dim(res)))}

matrixEffort <- function(effort)
    matrix(apply(effort,2,as.double),dim(effort))



