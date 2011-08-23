lsm <- function(stock, index, fratio=1, fit.plusgroup=T) {
  harvest(stock)[, ac(range(stock)["maxyear"])] <- 0.5
  diff <- 1
  while (diff > 1e-06) {
    stock <- stock + VPA(stock, fratio = fratio)
    ages <- range(index)["min"]:range(index)["max"]
    yrs <- range(index)["minyear"]:range(index)["maxyear"]
    stk <- trim(stock, year = yrs, age = ages)
    Cp <- catch.n(index)/catch.n(stk)
    q <- sweep(Cp * harvest(stk), 2, effort(index), "/")
    gmq <- apply(q, 1, function(x) exp(mean(log(x), na.rm = T)))
    mFp <- gmq * c(apply(effort(index), 1, mean))
    Fr <- mFp * (apply(Cp, 1, mean, na.rm = T))^-1
#    Fnew <- c(Fr, rep(Fr[ac(max(ages)), ], 2))
    Fnew <- Fr
    diff <- sum(abs(harvest(stock)[, ac(range(stock)["maxyear"])] - Fnew))
    harvest(stock)[, ac(range(stock)["maxyear"])] <- c(Fnew)
  }
  res <- VPA(stock, fratio=fratio, fit.plusgroup=fit.plusgroup)
  index.res(res) <- FLQuants(q)
  return(res)
}
