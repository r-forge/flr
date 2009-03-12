# project - projection code
# FLAssess/R/project.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas & Robert Scott, JRC
# $Id$

# TODO 2009.03.10 imosqueira
# - Add option to fill one more year
# - Separate catches on landings/discards

if (!isGeneric("project"))
	setGeneric("project", function(stock, sr, harvest, catch, ...)
		standardGeneric("project"))

# project(stock=FLStock, sr=FLSR, harvest=missing, catch=FLQuant)  {{{
setMethod("project", signature(stock="FLStock", sr="FLSR", harvest="missing",
  catch="FLQuant"),
  function(stock, sr, catch, years,
    sel=yearMeans(harvest(stock)[,ac(seq(years[1]-1, by=-1, length=3))]),...)
  {
    # catch must have quant='all'
    if(dim(catch)[1] > 1)
      stop("'catch' must be in weight per year for all ages")

    # get iters
    iters <- max(dims(stock)$iter, dims(catch)$iter)

    harvest(stock)[,ac(years)] <- as.numeric(NA)
    harvest <- harvest(stock)[,ac(years)]

    foo <- function(target, iter)
      as.numeric(project(stock=iter(stock, iter), sr=iter(sr, iter), harvest=target,
        years=i, sel=iter(sel, iter))@catch[, ac(i)]-iter(catch[,ac(i)], iter))
    
    for (i in years)
    {
      for (j in seq(iters))
      {
      iter(harvest, j)[,ac(i)] <- uniroot(foo, c(1e-8, 4), iter=j, f.lower=-1e-8,
          f.upper=foo(4, j), tol=0.001)$root
      }
      harvest(stock)[,ac(i)] <- harvest[, ac(i)] * (sel / as.numeric(quantMeans(sel[ac(seq(range(stock, 'minfbar'), range(stock, 'maxfbar')))])))
      stock <- project(stock=stock, sr=sr, harvest=harvest(stock)[, ac(i)],
      years=ac(i), ...)
     }
    return(stock)
  }
) # }}}

# project(stock=FLStock, sr=FLSR, harvest=missing, catch=numeric)  {{{
setMethod("project", signature(stock="FLStock", sr="FLSR", harvest="missing",
  catch="numeric"),
  function(stock, sr, catch, years,
    sel=yearMeans(harvest(stock)[,ac(seq(years[1]-1, by=-1, length=3))]),...)
  {
    project(stock=stock, sr=sr, catch=FLQuant(catch, dimnames=list(year=years)), years=years, sel=sel, ...)
  }
) # }}}

# project(stock=FLStock, sr=FLSR, harvest=missing, catch=missing) {{{
setMethod("project", signature(stock="FLStock", sr="FLSR", harvest="missing", catch="missing"),
  function(stock, sr, years, ...)
  {
    # harvest assumed to be in stock
    harvest <- harvest(stock)[, ac(years)]
    project(stock=stock, sr=sr, harvest=harvest, years=years, ...)
  }
) # }}}

# project(stock=FLStock, sr=FLSR, harvest=numeric, catch=missing) {{{
setMethod("project", signature(stock="FLStock", sr="FLSR", harvest="numeric", catch="missing"),
  function(stock, sr, harvest, years,
    # selectivity default as mean of last 3 years
    sel=yearMeans(harvest(stock)[,ac(seq(years[1]-1, by=-1, length=3))]),
    scaled=FALSE, ...)
  {
    # scale sel so that fbar(sel) equals fbar(harvest(last_year))
    if(scaled)
      sel <- sel * as.vector(quantMeans(harvest(stock)[ac(seq(range(stock, 'minfbar'),
        range(stock, 'maxfbar'))),ac(years[1]-1)]) / quantMeans(sel[ac(seq(range(stock,
        'minfbar'), range(stock, 'maxfbar'))),]))
    
    # get right dimnames for sel
    har <- FLQuant(dimnames=c(dimnames(sel)[c(1,3:6)], list(year=years)))
    har[] <- sel

    # call project(stock, sr, harvest=FLQuant)
    project(stock, sr, har/as.numeric(quantMeans(har[ac(seq(range(stock, 'minfbar'), 
      range(stock, 'maxfbar')))])) * harvest, years=years)

  }
) # }}}

# project(stock=FLStock, sr=FLSR, harvest=FLQuant, catch=missing) {{{
setMethod("project", signature(stock="FLStock", sr="FLSR", harvest="FLQuant", 
  catch="missing"),
    function(stock, sr, harvest, years, ...)
  {
    # turn harvest quant into numeric if needed
    if(dim(harvest)[1] == 1 & dims(stock)[dims(stock)$quant] > 1)
      return(project(stock=stock, sr=sr, harvest=as.numeric(harvest), years=years, ...))
    
    # get rec.age
    rec.age <- as.numeric(dimnames(rec(sr))$age[1])
    
    # does ages in mat include rec.age
    if(rec.age == 0)
      if(any(mat(stock)['0', ac(years[1])]> 0))
        stop("Cannot project for a stock with mature fish at age='0'")

      # TODO add extra year if in object
      for(i in as.character(years))
      {
        pyear <- ac(as.numeric(i)-1)
        
        for(j in dimnames(stock.n(stock))$age[-1])
        {
          page <- ac(as.numeric(j)-1)
          # N_t+1,a = N_t,a * e ^ (-M_t,a * F_t,a)
          stock.n(stock)[j, i] <- stock.n(stock)[page, pyear] * exp(-(m(stock)
            [page, pyear] + harvest(stock)[page, pyear]))
        }
        # abundance at first age
        # TODO Bad hack to get geomean to work. Need to dispatch properly
        if (identical(ac(model(sr)), ac(rec~a)))
          stock.n(stock)[1,i] <- predict(sr)
        else
          stock.n(stock)[1,i] <- predict(sr, ssb=ssb(stock)[,ac(as.numeric(i)-rec.age)])
        
        # plusgroup
        pgroup <- range(stock, 'plusgroup')
        if(!is.na(pgroup))
          stock.n(stock)[ac(pgroup),i] <- stock.n(stock)[ac(pgroup),i] +
          stock.n(stock)[ac(pgroup), pyear] * exp(-(m(stock)[ac(pgroup), pyear] +
          harvest(stock)[ac(pgroup), pyear]))
        
        # catch.n
        catch.n(stock)[, i] <- harvest[, i] / (harvest[, i] + m(stock)[, i]) *
          stock.n(stock)[, i] * (1 - exp(-(harvest[, i] + m(stock)[, i])))

      }
      # catch
      catch(stock) <- computeCatch(stock)
      
      # harvest
      harvest(stock)[, ac(years)] <- harvest[, ac(years)]

      # TODO landings & discards
      # (1) apply mean proportions-at-age

      return(stock)
  }
) # }}}

# project(stock=FLStock, sr=missing, harvest=ANY, catch=ANY) {{{
setMethod('project', signature(stock='FLStock', sr='missing', harvest='ANY', 
  catch='missing'),
  function(stock, harvest, years, frec=exp(yearMeans(log(rec(stock)
    [,!dimnames(catch(stock))$year %in% ac(years)]), na.rm=TRUE))
    , ...)
  {
    sr <- as.FLSR(stock, model=geomean)
    
    # geomean of all series
    params(sr) <- FLPar(as.numeric(frec))
    project(stock=stock, sr=sr, harvest=harvest, years=years, ...)
  }
) # }}}

# project(stock=FLStock, sr=missing, harvest=missing, catch=ANY) {{{
setMethod('project', signature(stock='FLStock', sr='missing', harvest='missing', 
  catch='ANY'),
  function(stock, catch, years,
    frec=exp(yearMeans(log(rec(stock)[,!dimnames(catch(stock))$year %in% ac(years)]),
    na.rm=TRUE)), ...)
  {
    sr <- as.FLSR(stock, model=geomean)
    
    # geomean of all series
    params(sr) <- FLPar(as.numeric(frec))
    project(stock=stock, sr=sr, catch=catch, years=years, ...)
  }
) # }}}
