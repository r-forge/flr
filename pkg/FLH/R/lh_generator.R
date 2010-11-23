

genBRP <- function(age, fmsy,
  growth=list(model=mass~a*(Linf*(1-exp(-k*(age-t0))))^b,
    params=list(Linf=NA, k=NA, t0=0, a=0.001, b=3)),
  sr=list(model=bevholtSV()$model, params=list(s=NA, v=NA)),
  selectivity=list(model = pr ~ dnormal(age, a, SL, sR), params=list(a=NA, sL=NA, sR=NA)),
  maturity=list(model = mat ~ logistic(age, mat50, mat95), params=list(mat95=NA)),
  m=list(model = m ~ m1 + h * Linf ^ i * L ^ n,
    params=list(m1=0.1, h=1.71, n=-1.66, i=0.8)))
{
  # dimensions
  # age <- 1:20
  dnames <- list(age=age)

  # biological values
  #   TODO formulas or functions
  # weights
  wts <- FLQuant(eval(as.list(growth$model)[[3]], c(growth$params, list(age=age))),
    dimnames=dnames)
  # M
  m <- FLQuant(eval(as.list(m$model)[[3]], c(list(L=1000*eval(as.list(growth$model)[[3]],
    c(growth$params, list(age=age+0.5)))), m$params, growth$params)),
    dimnames=dnames)
  # mat@50%
  mat50 <- log(((3 * growth$params$k + mean(m)) / mean(m)) / growth$params$k)

  # selection pattern
  sel <- FLQuant(eval(as.list(selectivity$model)[[3]],
    list(age=age, a=(mat50+maturity$params$mat95)*selectivity$params$a,
        sL=(mat50+maturity$params$mat95)*selectivity$params$sL,
        sR=(mat50+maturity$params$mat95)*selectivity$params$sR)), dimnames=dnames)

  # maturity
  mat <- FLQuant(eval(as.list(maturity$model)[[3]], c(list(mat50=mat50),
    maturity$params)), dimnames=dnames)

  # create FLBRP
  res <- FLBRP(stock.wt = wts,
    landings.wt = wts,
    discards.wt= wts,
    bycatch.wt     =wts,
    m = m,
    mat = mat,
    landings.sel = sel,
    discards.sel = FLQuant(0,dimnames=dnames),
    bycatch.harvest = FLQuant(0,dimnames=dnames),
    harvest.spwn = FLQuant(0,dimnames=dnames),
    m.spwn = FLQuant(0,dimnames=dnames),
    availability = FLQuant(1,dimnames=dnames))

  # SR
  params(res) <- FLPar(do.call(abPars, c(list(model=SRModelName(sr$model),
    spr0=spr0(res)), sr$params)))
  model(res) <- abModel(sr$model)

  # brp
  res        <-brp(res)

  # equilibrium conditions at MSY (?)
  fbar(res)[] <- fmsy * refpts(res)["msy","harvest",1]

  return(res)
}

res <- genBRP(age=1:20, fmsy=1, 
  growth=list(model=mass~a*(Linf*(1-exp(-k*(age-t0))))^b,
  params=list(Linf=10, k=0.1, t0=0.01, a=0.001, b=3)),
  selectivity=list(model = pr ~ dnormal(age, a, sL, sR), params=list(a=8, sL=4, sR=12)),
  maturity=list(model = mat ~ logistic(age, mat50, mat95), params=list(mat95=6)),
  sr=list(model=bevholtSV()$model, params=list(s=0.6, v=2000)),
  m=list(model = m ~ m1 + h * Linf ^ i * L ^ n,
    params=list(m1=0.1, h=1.71, n=-1.66, i=0.8)))

plot(res)
