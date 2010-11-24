# lh_generator - «Short one line description»
# FLH/R/lh_generator.R

# Copyright 2010 Iago Mosqueira & Finlay Scott, Cefas. Distributed under the GPL 2
# $Id:  $

# Reference:
# Notes:

# TODO Wed 24 Nov 2010 09:08:09 AM CET IM:

genBRP <- function(age, Linf, k, s, v, a1, sL, sR, mat95,
  growth=mass~a*(Linf*(1-exp(-k*(age-t0))))^b, sr=bevholtSV()$model,
  sel=pr ~ dnormal(age, a1, sL, sR),
  # Gislason 2008
  m=m ~ m1 + h * Linf ^ i * L ^ n,
  mat=mat ~ logistic(age, mat50, mat95), ...)
  {
  # params with defaults included
  params <- list(Linf=Linf, k=k, t0=0, a=0.001, b=3, fmsy=1,
    s=s, v=v, a1=a1, sR=sR, sL=sL, mat95=mat95, m1=0.1, h=1.71, n=-1.66, i=0.8)

  # extract ...
  args <- list(...)
  if(length(args) > 0)
    params [names(args)] <- args
  
  # dimensions
  # age <- 1:20
  dnames <- list(age=age)

  # biological values
  #   TODO formulas or functions
  # weights
  wts <- FLQuant(eval(as.list(growth)[[3]], c(params, list(age=age))), dimnames=dnames)
  # M
  m <- FLQuant(eval(as.list(m)[[3]], c(params, list(L=(1000*eval(as.list(growth)[[3]],
    c(params, list(age=age+0.5)))) ^(1/3)))), dimnames=dnames)
  # mat@50%
  # TODO Ref!
  mat50 <- log(((3 *params$k + mean(m)) / mean(m)) / params$k)

  # selection pattern
  sell <- FLQuant(eval(as.list(sel)[[3]], list(age=age,
    a1=(mat50+params$mat95)*params$a1, sL=(mat50+params$mat95)*params$sL,
    sR=(mat50+params$mat95)*params$sR)), dimnames=dnames)

  # maturity
  matt <- FLQuant(eval(as.list(mat)[[3]], c(list(mat50=mat50),
    params)), dimnames=dnames)

  # create FLBRP
  res <- FLBRP(stock.wt = wts,
    landings.wt = wts,
    discards.wt= wts,
    bycatch.wt     =wts,
    m = m,
    mat = matt,
    landings.sel = sell,
    discards.sel = FLQuant(0,dimnames=dnames),
    bycatch.harvest = FLQuant(0,dimnames=dnames),
    harvest.spwn = FLQuant(0,dimnames=dnames),
    m.spwn = FLQuant(0,dimnames=dnames),
    availability = FLQuant(1,dimnames=dnames))

  # SR
  params(res) <- FLPar(do.call(abPars, c(list(model=SRModelName(sr),
    spr0=spr0(res)), params[c('s','v')])))
  model(res) <- abModel(sr)

  # brp
  res        <-brp(res)

  # equilibrium conditions at MSY (?)
  # TODO Ref!
  fbar(res)[] <- params$fmsy * refpts(res)["msy","harvest",1]

  return(res)
}
