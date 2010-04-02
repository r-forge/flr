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
      kobe(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass), refpts=biomass@refpts)
    }
) # }}}

# kobe(FLQuant, FLQuant, refpts) {{{
setMethod("kobe", signature(biomass="FLQuant", harvest="FLQuant", refpts="refpts"),
  function(biomass, harvest, refpts, xlab=expression(SSB:B[MSY]),
      ylab=expression(F:F[MSY]), ...)
  {
    #
    biomass <- biomass / refpts['msy', 'ssb']
    harvest <- harvest / refpts['msy', 'harvest']

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
    lines(biomass, harvest)
    points(biomass, harvest, pch=21, bg=gray(seq(0.9, 0.2, length=length(biomass))),
        cex=1.5)
    points(biomass[,lastyear], harvest[,lastyear], cex=1.5, col='white')
    text(biomass[,lastyear]+biomass[,lastyear]*0.1, harvest[,lastyear], lastyear)
    text(biomass[,firstyear]+biomass[,firstyear]*0.1, harvest[,firstyear], firstyear)
  }
) # }}}

# kobeProb

# kobeMatrix
