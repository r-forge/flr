# kobe - «Short one line description»
# kobe

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

setGeneric('kobe', function(biomass, harvest, refpts, ...)
	standardGeneric('kobe'))

# kobe(FLBRP) {{{
setMethod("kobe", signature(biomass="FLBRP", harvest="missing", refpts="missing"),
    function(biomass, ...)
    {
      kobe(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass), refpts=biomass@refpts,
        ...)
    }
) # }}}

# kobe(FLBRP) {{{
setMethod("kobe", signature(biomass="FLBRP", harvest="missing", refpts="list"),
    function(biomass, refpts, ...)
    {
      kobe(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass), refpts=refpts,
        ...)
    }
) # }}}

# kobe(FLQuant, FLQuant, list) {{{
setMethod("kobe", signature(biomass="FLQuant", harvest="FLQuant", refpts="list"),
  function(biomass, harvest, refpts, ...)
  {
     #
      if(any(!names(refpts) %in% c('ssb', 'harvest')))
        stop("refpts list must have elements named 'ssb' and 'harvest'")
      res <- refpts(refpt='msy', iter=max(unlist(lapply(refpts, length))))
      res[,'ssb'] <- refpts$ssb
      res[,'harvest'] <- refpts$harvest
      kobe(biomass, harvest, res, ...)
  }
) # }}}

# kobe(FLQuant, FLQuant, refpts) {{{
setMethod("kobe", signature(biomass="FLQuant", harvest="FLQuant", refpts="refpts"),
  function(biomass, harvest, refpts, xlab=expression(SSB:SSB[MSY]),
      ylab=expression(F:F[MSY]), ...)
  {
    # check refpts
    if(dim(refpts)[1] > 1)
      stop("kobe can only plot relative to a single reference point")
    
    # check dims
    dims <- c(biomass=dim(biomass)[6], harvest=dim(harvest)[6], refpts=dim(refpts)[3])
    # not all 1?
    if(!sum(dims) == 3)
      if(any(dims[dims!=1] != max(dims[dims!=1])))
        stop("iters in input objects do not match")

    #
    if(dims['biomass'] > dims['harvest'])
      harvest <- propagate(harvest, dims['biomass'], fill.iter=TRUE)
    if(dims['biomass'] < dims['harvest'])
      biomass <- propagate(biomass, dims['harvest'], fill.iter=TRUE)

    # reshape biomass & harvest if refpts has iters and they don't
    if(dims['refpts'] > 1 & dims['biomass'] == 1)
      biomass <- propagate(biomass, dims['refpts'], fill.iter=TRUE)
    if(dims['refpts'] > 1 & dims['harvest'] == 1)
      harvest <- propagate(harvest, dims['refpts'], fill.iter=TRUE)

    # indices
    biomass <- sweep(biomass, 6, refpts[, 'ssb'], '/', check.margin=FALSE)
    mbiomass <- apply(biomass, 1:5, median)
    harvest <- sweep(harvest, 6, refpts[, 'harvest'], '/', check.margin=FALSE)
    mharvest <- apply(harvest, 1:5, median)

    # limits
    xlim <- c(min(0.5, min(biomass)), max(1.5, max(biomass)))
    ylim <- c(min(0.5, min(harvest)), max(1.5, max(harvest)))
    firstyear <- dimnames(biomass)$year[1]
    lastyear <- dimnames(biomass)$year[dim(biomass)[2]]

    # initial plot
    plot(1, 1, type='b', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)

    # actual limits
    parlim<- par()$usr

    # polygons
    # green
    polygon(c(1, 1, parlim[2], parlim[2]), c(parlim[3], 1, 1, parlim[3]), col='green')
    # yellow
    polygon(c(1, parlim[1], parlim[1], 1), c(parlim[3], parlim[3], 1, 1), col='yellow')
    # red
    polygon(c(parlim[1], parlim[1], 1, 1), c(1, parlim[4], parlim[4], 1), col='red')
    # orange
    polygon(c(1, 1, parlim[2], parlim[2]), c(1, parlim[4], parlim[4], 1), col='orange')

    # points
    lines(mbiomass, mharvest)
    points(biomass[,lastyear], harvest[,lastyear], cex=1, pch=21, bg='blue')
    points(mbiomass, mharvest, pch=21, bg=gray(seq(0.9, 0.2, length=length(biomass))),
      cex=1.5)
    text(mbiomass[,lastyear]+mbiomass[,lastyear]*0.1, mharvest[,lastyear], lastyear)
    text(mbiomass[,firstyear]+mbiomass[,firstyear]*0.1, mharvest[,firstyear], firstyear)
  }
) # }}}

# kobeProb

# kobeMatrix
