# kobe - «Short one line description»
# kobe

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, Cefas & Santiago Cerviño, IEO
# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# kobe
# kobe(FLBRP, missing, missing) {{{
setMethod("kobe", signature(biomass="FLBRP", harvest="missing", refpts="missing"),
    function(biomass, ...)
      kobe(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass),refpts=biomass@refpts['msy',], ...))

# kobe(FLBRP,missing,list) {{{
setMethod("kobe", signature(biomass="FLBRP", harvest="missing", refpts="ANY"),
    function(biomass, refpts, xlim=c(0,0),ylim=c(0,0),...)
      kobe(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass), refpts=refpts,...))

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
      
     # call kobe(FLQuant, FLQuant, refpts)
      kobe(biomass, harvest, res, ...)
  })

# kobe(FLQuant, FLQuant, refpts) {{{
setMethod("kobe", signature(biomass="FLQuant", harvest="FLQuant", refpts="refpts"),
  function(biomass, harvest, refpts, type='b', lines=list(col='black'),
      points=list(pch=21, bg=gray(seq(0.8, 0.2, length=dim(biomass)[2])), cex=1.5), 
      xlab=expression(SSB:SSB[MSY]), ylab=expression(F:F[MSY]), add=FALSE,  ...)
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
    xlim    <-range(harvest)
    xlim[1] <- 0
    xlim[2] <- min(xlim[2],max(1.5, min(biomass)))

    ylim    <-range(biomass)
    ylim[1] <- 0
    ylim[2] <- max(ylim[2],max(1.5, max(harvest)))

    firstyear <- dimnames(biomass)$year[1]
    lastyear <- dimnames(biomass)$year[dim(biomass)[2]]

    if(!add)
    {
      # initial plot
      plot(1, 1, type=type, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)

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
    
      # year labels
      text(mbiomass[,lastyear]+mbiomass[,lastyear]*0.1, mharvest[,lastyear], lastyear)
      text(mbiomass[,firstyear]+mbiomass[,firstyear]*0.1, mharvest[,firstyear], firstyear)
    }

    # points
    if(type %in% c('l', 'b'))
      do.call('lines', c(list(x=mbiomass, y=mharvest), lines))

    if(type %in% c('p', 'b'))
    {
      points(biomass[,lastyear], harvest[,lastyear], cex=1, pch=21, bg='blue')
      do.call('points', c(list(x=mbiomass, y=mharvest), points))
    }
  }
) # }}}

# kobe(data.frame, missing, list)
setMethod("kobe", signature(biomass="data.frame", harvest="missing", refpts="list"),
   function(biomass, refpts, ...)
   {
     # check df names
     if(any(!names(biomass) %in% c("year", "biomass", "ssb", "harvest")))
       stop("data.frame must have names 'year', 'ssb/biomass' and 'harvest'")
     if(all(c("biomass", "ssb") %in% names(biomass)))
       stop("columns named 'biomass' and 'ssb' are both present in data.frame")

     # select biomass or ssb
     ssb <- FLQuant(biomass[,names(biomass) %in% c("biomass", "ssb")],
         dimnames=list(age="all", year=biomass$year))
     harvest <- FLQuant(biomass$harvest, dimnames=list(age="all", year=biomass$year))

     kobe(ssb, harvest, refpts=refpts, ...)
    
   }
)

# kobeProb
# kobeProb(FLBRP)  {{{
setMethod('kobeProb', signature(biomass='FLBRP', harvest='missing', refpts='missing'),
  function(biomass, ylab='probability', xlab='', lwd=1.5, ...)
  {
    kobeProb(biomass=ssb.obs(biomass), harvest=fbar.obs(biomass),
      refpts=biomass@refpts['msy',], xlab=xlab, ylab=ylab, lwd=lwd, ...)
  }
) # }}}

# kobeProb(FLQuant, FLQuant, list) {{{
setMethod("kobeProb", signature(biomass="FLQuant", harvest="FLQuant", refpts="list"),
  function(biomass, harvest, refpts, ...)
  {
     #
      if(any(!names(refpts) %in% c('ssb', 'harvest')))
        stop("refpts list must have elements named 'ssb' and 'harvest'")
      res <- refpts(refpt='msy', iter=max(unlist(lapply(refpts, length))))
      res[,'ssb'] <- refpts$ssb
      res[,'harvest'] <- refpts$harvest
      kobeProb(biomass, harvest, res, ...)
  }
) # }}}

# kobeProb(FLQuant, FLQuant, refpts)  {{{
setMethod('kobeProb', signature(biomass='FLQuant', harvest='FLQuant', refpts='refpts'),
  function(biomass, harvest, refpts, xlab='probability', ylab='', lwd=1.5, ...)
  {
    # check refpts
    if(dim(refpts)[1] > 1)
      stop("kobe can only plot relative to a single reference point")

    # check dims
    dims <- c(biomass=dim(biomass)[6], harvest=dim(harvest)[6], refpts=dim(refpts)[3])

    # refpts not 1?
    if(dims['refpts'] > 1)
      stop('kobeProb can only deal with a single reference point')

    if(dims[1] == 1 & dims[2] == 1)
      warning('no iters in biomass or harvest, plot might not make sense')

    # biomass & harvest not equal?
    dimt <- dims['biomass'] / dims['harvest']
    if(dimt != 1 & dimt != max(dims[1:2]))
      stop('biomass and harvest iters do not match')

    iters <- max(dims)
    
    # indices
    biomass <- sweep(biomass, 6, refpts[, 'ssb'], '/', check.margin=FALSE)
    harvest <- sweep(harvest, 6, refpts[, 'harvest'], '/', check.margin=FALSE)
    years <- dimnames(biomass)$year

    # red
    red <- apply(biomass < 1 & harvest < 1, 1:5, sum) / iters

    # green
    green <- apply(biomass > 1 & harvest > 1, 1:5, sum) / iters

    # yellow
    yellow <- apply((biomass > 1 & harvest < 1) | (biomass < 1 & harvest > 1),
      1:5, sum) / iters

    # plot
    plot(years, red, type='l', col='red', lwd=lwd, ylim=c(0,1), xlab=xlab, ylab=ylab)
    lines(years, yellow, col='yellow', lwd=lwd)
    lines(years, green, col='green', lwd=lwd)
  }
) # }}}

# kobeMatrix

KSMinterpProb<-function(object,x0,y0){
    object<-defactor(cbind(expand.grid(dimnames(object)[c("TAC","year")]),val=c(object)))
    object<-interp(object[,2],object[,1],object[,3], xo=x0,
                                                     yo=y0)

    dimnames(object[[3]])<-list(year=x0,TAC=y0)

    return(object[[3]])}
