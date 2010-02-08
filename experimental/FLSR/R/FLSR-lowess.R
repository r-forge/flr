# lowess  {{{
setMethod('lowess', signature(x='FLSR', y='missing', f='ANY', delta='ANY', iter='ANY'),
  function(x, f=2/3, iter=3, delta=0.01 * diff(range(ssb(x))))
  {
    res <- lowess(rec(x)~ssb(x), f=f, delta=delta, iter=iter)
    idx <- order(as.numeric(ssb(x)))
    fitted(x) <- FLQuant(res$y[idx], dimnames=dimnames(ssb(x)))
    residuals(x) <- log(rec(x)/fitted(x))
    model(x) <- rec~FLQuant(lowess(ssb, rec)$y[order(as.numeric(ssb))],
      dimnames=dimnames(rec))
    params(x) <- FLPar()
    return(x)
  }
) # }}}
