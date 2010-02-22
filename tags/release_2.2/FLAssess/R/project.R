
# proj3.r code
# FLAssess/R/project.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, CEFAS; Robert Scott, JRC
# 

# TODO
# 

control.check <- function(stock, control, years) {

}


# function to retrieve the discards partition information from the discards.n slot
get.dp <- function(stock, years) {

    dp        <- discards.n(stock)[,ac(years)]  #landings.n(stock)[,ac(years)]/catch.n(stock)[,ac(years)]
    dp[is.na(dp)]       <- 0     # ie. where 0/0 - discards and catch = 0
    dp[is.infinite(dp)] <- 1     # ie. where 2/0 - discards true and catch zero
    
    return(dp)
}


# function to calculate catch landings and dicards from the discards partition
calc.discards <- function(stock, years, dp) {
             
  if(!is(dp, "FLQuant"))
    stop("error calculating discards: dp must be an FLQuant")
  if(any(is.na(dp)))
    stop("error calculating discards: you have NAs for discard proportions")
  if(any(is.infinite(dp)))
    stop("error calculating discards: you have infinite values for discard propotions")
  if(any(dp > 1))
    stop("error calculating discards: discard propns greater than 1")
  if(!all(dimnames(dp)$year == ac(years)))
    stop("error calculating discards: year range of dp != year range of projection")

  landings.n(stock)[,ac(years)] <- catch.n(stock)[,ac(years)] * (1-dp)
  discards.n(stock)[,ac(years)] <- catch.n(stock)[,ac(years)] * dp

  catch(stock)[,ac(years)]    <- computeCatch(stock)[,ac(years)] 
  landings(stock)[,ac(years)] <- computeLandings(stock)[,ac(years)] 
  discards(stock)[,ac(years)] <- computeDiscards(stock)[,ac(years)] 

  return(stock)
}


basic.proj <-  function(stock, sr, years)
  {
   ages <- ac(range(stock)["min"]:range(stock)["max"])
   pg   <- range(stock)["plusgroup"]
   
   for(i in 1:dims(stock)$iter){                #for each iteration
#    stk <- iter(stock, i)
    
    for(y in years){
     newN <- stock.n(stock)[ages,ac(y-1),,,,i]*exp(-harvest(stock)[ages,ac(y-1),,,,i]-m(stock)[ages,ac(y-1),,,,i])

     if(!is.na(pg))
       newN[ac(pg-1)] <- newN[ac(pg-1)] + newN[ac(pg)] 

     ifelse(!is.na(stock.n(stock)[1,ac(y),,,,i]), 
        newN <- c(stock.n(stock)[1,ac(y),,,,i],newN),                   # if a rec value exists already use it
       ifelse(any(is.element(all.vars(model(sr)),"ssb")),
         newN <- c(predict(sr, ssb=ssb(stock)[,ac(y-1),,,,i]), newN),   # if a stock recruit function exists
         newN <- c(predict(sr), newN)))                                 # if a gm rec value

     stock.n(stock)[,ac(y),,,,i] <- newN[1:length(ages)]
    }
#   iter(stock,i) <- stk
   }
     
   catch.n(stock)[,ac(years)] <- harvest(stock)[,ac(years)]/(harvest(stock)[,ac(years)]+m(stock)[,ac(years)]) * 
                                 stock.n(stock)[,ac(years)] * 
                                 (1 - exp(-harvest(stock)[,ac(years)] - m(stock)[,ac(years)]))
   return(stock) 
  }  #/// end basic.proj



catch.constraint <-   function(stock, sr, catch, years){
    # catch must have quant='all'
    if(dim(catch)[1] > 1)
      stop("'catch' must be in weight per year for all ages")
    if(any(dimnames(catch)$year!=ac(years)))
      stop("year range of catch constraints must be the same as the projection year range") 

    stock    <- basic.proj(stock=stock, sr=sr, years=years)
    years.cc <- dimnames(catch[!is.na(catch)])$year
    iters    <- dims(stock)$iter
    catch    <- propagate(catch, iters)

    for (i in years.cc){
      for (j in seq(iters)) {
      
      n  <- stock.n(stock)[,ac(i),,,,j]
      f  <- harvest(stock)[,ac(i),,,,j]
      m  <- m(stock)[,ac(i),,,,j]
      wt <- catch.wt(stock)[,ac(i),,,,j]
      F  <- 5.0
      catch.eq <- expression(sum((F*f)/(F*f+m)*n*(1-exp(-(F*f+m)))*wt, na.rm=TRUE))
      TAC  <- catch[,i,,,,j]
      diff <- 1

      while(abs(diff) > 0.000001) {
        Fold <- F
        Y  <- as.numeric(eval(catch.eq) - TAC)
        dY <- sum(f*n*wt*(m-exp(-F*f-m)*m+exp(-F*f-m)*F^2*f^2+exp(-F*f-m)*F*f*m)/(F*f+m)^2, na.rm=TRUE)
        diff <- Y/dY
        F <- Fold - diff
      }      
      harvest(stock)[,i,,,,j] <- harvest(stock)[,i,,,,j] * F
      }
    stock <- basic.proj(stock=stock, sr=sr, years=years)
    }
    return(stock)
}



landings.constraint <-   function(stock, sr, landings, dp, years){
    # landings must have quant='all'
    if(dim(landings)[1] > 1)
      stop("'landings' must be in weight per year for all ages")
    if(any(dimnames(landings)$year!=ac(years)))
      stop("year range of landings constraints must be the same as the projection year range") 

    stock    <- basic.proj(stock=stock, sr=sr, years=years)
    years.lc <- dimnames(landings[!is.na(landings)])$year
    iters    <- dims(stock)$iter
    landings <- propagate(landings, iters)
#    dp       <- propagate(dp, iters)

    for (i in years.lc){
      for (j in seq(iters)) {
      
      n  <- stock.n(stock)[,ac(i),,,,j]
      f  <- harvest(stock)[,ac(i),,,,j] 
      fl <- harvest(stock)[,ac(i),,,,j]*(1-dp[,ac(i),,,,j])
      m  <- m(stock)[,ac(i),,,,j]
      wt <- landings.wt(stock)[,ac(i),,,,j]
      F  <- 5.0
      catch.eq <- expression(sum((F*fl)/(F*f+m)*n*(1-exp(-(F*f+m)))*wt, na.rm=TRUE))
      TAC  <- landings[,i,,,,j]
      diff <- 1

      while(abs(diff) > 0.000001) {
        Fold <- F
        Y  <- as.numeric(eval(catch.eq) - TAC)
        dY <- sum(fl*n*wt*(m-exp(-F*f-m)*m+exp(-F*f-m)*F^2*f^2+exp(-F*f-m)*F*f*m)/(F*f+m)^2, na.rm=TRUE)
        diff <- Y/dY
        F <- Fold - diff
      }      
      harvest(stock)[,i,,,,j] <- harvest(stock)[,i,,,,j] * F
      }
    stock <- basic.proj(stock=stock, sr=sr, years=years)
    }
    return(stock)
}



ssb.constraint  <-  function(stock, sr, ssb, years){

  # ssb must have quant='all'
    if(dim(ssb)[1] > 1)
      stop("'ssb' must be in weight per year for all ages")
    if(any(dimnames(catch)$year!=ac(years)))
      stop("year range of ssb constraints must be the same as the projection year range")

    stock    <- basic.proj(stock, sr, years)
    iters    <- dims(stock)$iter
    years.cc <- as.numeric(dimnames(ssb[!is.na(ssb)])$year)
    ssb      <- propagate(ssb, iters)

    if(any(years.cc == years[1]))
      stop("Oops: cannot calculate an SSB constraint for the first projection year")
    
    for (i in years.cc){
      for (j in seq(iters)) {

      if(any(is.element(all.vars(model(sr)),"ssb")))       
        rec <- predict(sr, ssb=ssb(stock)[,ac(i-1)],,,,j)  #if a stock recruit function exists

      if(!any(is.element(all.vars(model(sr)),"ssb")))       
        rec <- predict(sr)                               #if no srr

      if(!is.na(stock.n(stock)[1,ac(i)]))                  # if rec value given in stock object already
        rec <- stock.n(stock)[1,ac(i)]

      if(any(harvest.spwn(stock)[,ac(i),,,,j] > 0)){
        n  <- c(rec,stock.n(stock)[,ac(i),,,,j])
        f  <- c(harvest(stock)[,ac(i),,,,j],harvest(stock)[range(stock)["max"],ac(i),,,,j])
        fsp<- c(harvest.spwn(stock)[,ac(i),,,,j],harvest.spwn(stock)[range(stock)["max"],ac(i),,,,j])
        m  <- c(m(stock)[,ac(i),,,,j],m(stock)[range(stock)["max"],ac(i),,,,j])
        msp<- c(m.spwn(stock)[,ac(i),,,,j],m.spwn(stock)[range(stock)["max"],ac(i),,,,j])
      }

      if(all(harvest.spwn(stock)[,ac(i),,,,j] == 0)){
        n  <- c(rec,stock.n(stock)[,ac(i-1),,,,j])
        f  <- c(0, harvest(stock)[,ac(i-1),,,,j])
        fsp<- c(rep(1,length(f)))
        m  <- c(0, m(stock)[,ac(i-1),,,,j])
        msp<- c(rep(1,length(m)))
      }
            
      wt <- c(stock.wt(stock)[,ac(i),,,,j],stock.wt(stock)[ac(range(stock)["max"]),ac(i),,,,j])
      mat<- c(mat(stock)[,ac(i),,,,j], mat(stock)[ac(range(stock)["max"]),ac(i),,,,j])
      F  <- 5.0

      ssb.eq      <- expression(sum(n*exp(-F*f*fsp-m*msp)*mat*wt))
      ssb.target  <- ssb[,ac(i),,,,j]
      diff        <- 1

      while(abs(diff) > 0.000001) {
        Fold <- F
        Y    <- as.numeric(eval(ssb.eq) - ssb.target)
        dY   <- sum(-f*fsp*n*exp(-F*f*fsp-m*msp)*mat*wt)  
        diff <- Y/dY
        F    <- Fold - diff
      }
      if(F<0) stop(paste("ssb constraint > max possible ssb in year ", i, ", iteration ", j))

      harvest(stock)[,ac(i-1),,,,j] <- harvest(stock)[,ac(i-1),,,,j] * F 
      }
    stock <- basic.proj(stock, sr, years)
    }
    return(stock)
}


mixed.constraints  <-  function(stock, sr, catch=FLQuant(NA), landings=FLQuant(NA), ssb=FLQuant(NA), dp, years){
      years.cc <- as.numeric(dimnames(catch[!is.na(catch)])$year)
      years.lc <- as.numeric(dimnames(landings[!is.na(landings)])$year)
      years.sc <- as.numeric(dimnames(ssb[!is.na(ssb)])$year)
      if(any(is.element(years.cc+1,years.sc)))
        stop("Oops: Cannot calculate an SSB constraint in year following a catch constraint")
      if(any(is.element(years.cc, years.lc)))
        stop("Oops: Cannot calculate a catch constraint and landings constraint in the same year")

      fun.seq <- data.frame(
         year  =c(years.cc, years.lc, years.sc),
         method=c(rep('catch',length(years.cc)),rep("landings",length(years.lc)) ,rep('ssb',length(years.sc))),
         value =c(as.numeric(catch[!is.na(catch)]),as.numeric(landings[!is.na(landings)]),as.numeric(ssb[!is.na(ssb)])))

      for(y in sort(c(years.cc, years.lc, years.sc),decreasing=F)){
        cdf       <- fun.seq[fun.seq$year==y,]
        cdf.order <- order(cdf$method, decreasing=T)
        for(k in cdf.order) {
          
          const <- FLQuant(NA, dimnames=list(age="all", year=ac(years[1]:y)))
          const[,ac(y),1,1,1,] <- cdf[k,"value"]      #fun.seq[fun.seq$year==y,"value"]
        
        if(cdf[k,"method"]=="catch")
          stock <- catch.constraint(stock=stock, sr=sr, catch=const, years=years[1]:y)
        if(cdf[k,"method"]=="ssb")
          stock <- ssb.constraint(stock=stock, sr=sr, ssb=const, years=years[1]:y)
        if(cdf[k,"method"]=="landings")
          stock <- landings.constraint(stock=stock, sr=sr, landings=const, dp=dp, years=years[1]:y)
        }
      }
      if(y < max(years))
        stock <- basic.proj(stock, sr, years)

      return(stock)
  }


if (!isGeneric("project"))
        setGeneric("project", function(stock, control, sr, ...)
                standardGeneric("project"))


# proj(stock=FLStock, control=projectControl, sr=FLSR)  {{{
setMethod("project", signature(stock="FLStock", control="projectControl", sr="FLSR"),
        function(stock, control, sr, ...){
     
     years <- c(control@target$year)
     iters <- dims(stock)$iter
     dp    <- get.dp(stock, years)
     
     for(i in iters){
    
       if(is.na(stock.n(stock)[1,ac(years[1]-1),,,,i])){
         if(any(is.element(all.vars(model(sr)),"ssb")))
            stock.n(stock)[1,ac(years[1]-1),,,,i] <- predict(sr, ssb=ssb(stock)[,ac(years[1]-2),,,,i])  #if SRR
         if(!any(is.element(all.vars(model(sr)),"ssb")))
           stock.n(stock)[1,ac(years[1]-1),,,,i] <- predict(sr)                               #if no SRR
       }
     }
     #change the f values first
     if(any(is.element(control@target$quantity, 'f'))) {
       yrs.f <- control@target[control@target$quantity=='f', "year"]
       val.f <- control@target[control@target$quantity=='f', "val"]
       mult  <- val.f/fbar(stock)[,ac(yrs.f)]
       harvest(stock)[,ac(yrs.f)] <- sweep(harvest(stock)[,ac(yrs.f)], 2:6, mult[,ac(yrs.f)], "*") 
     }
     
     if(any(is.element(control@target$quantity, c('catch', 'landings', 'ssb')))) {
       catch.q <- FLQuant(NA, dimnames=list(age="all", year=years))
       catch.q[,ac(control@target[control@target$quantity=='catch', "year"])] <- 
                       control@target[control@target$quantity=='catch', "val"]
       landings.q <- FLQuant(NA, dimnames=list(age="all", year=years))
       landings.q[,ac(control@target[control@target$quantity=='landings', "year"])] <-
                          control@target[control@target$quantity=='landings', "val"]
       ssb.q <- FLQuant(NA, dimnames=list(age="all", year=years))
       ssb.q[,ac(control@target[control@target$quantity=='ssb', "year"]+1)] <-
                       control@target[control@target$quantity=='ssb', "val"]      
 
       return(calc.discards(mixed.constraints(stock, sr, catch=catch.q, landings=landings.q, ssb=ssb.q, 
                                              dp=dp, years=years),years, dp)) 
     }
     return(calc.discards(basic.proj(stock, sr, years),years, dp))
   }
)




#project(stock=FLStock, control=missing, sr=FLSR)  {{{
setMethod("project", signature(stock="FLStock", control="missing", sr="FLSR"),
        function(stock, sr, years, ...){


     dp    <- get.dp(stock, years)

    if(is.na(stock.n(stock)[1,ac(years[1]-1)])){
      if(any(is.element(all.vars(model(sr)),"ssb")))
        stock.n(stock)[1,ac(years[1]-1)] <- predict(sr, ssb=ssb(stock)[,ac(years[1]-2)])  #if SRR
      if(!any(is.element(all.vars(model(sr)),"ssb")))
        stock.n(stock)[1,ac(years[1]-1)] <- predict(sr)                               #if no SRR
    }
    return(calc.discards(basic.proj(stock, sr, years),years, dp))
  }
)




if (!isGeneric("projecter"))
        setGeneric("projecter", function(stock, sr, catch, landings, ssb, ...)
                standardGeneric("projecter"))


# projecter(stock=FLStock, sr=FLSR)  {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="missing", landings="missing", ssb="missing"),
   function(stock, sr, years, ...){
     dp  <- get.dp(stock, years)
     return(calc.discards(basic.proj(stock, sr, years),years, dp))     
   }
)


# projecter(stock=FLStock, sr=FLSR, catch=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="FLQuant", landings="missing", ssb="missing"),
   function(stock, sr, catch, years, ...){
     dp <- get.dp(stock, years)
     return(calc.discards(catch.constraint(stock=stock, sr=sr, catch=catch, years=years),years,dp))     
   }
)


# projecter(stock=FLStock, sr=FLSR, ssb=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="missing", landings="missing", ssb="FLQuant"),
   function(stock, sr, ssb, years, ...){
     dp <- get.dp(stock, years)
     return(calc.discards(ssb.constraint(stock=stock, sr=sr, ssb=ssb, years=years),years,dp))     
   }
)


# projecter(stock=FLStock, sr=FLSR, landings=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="missing", landings="FLQuant", ssb="missing"),
   function(stock, sr, landings, years, ...){
     dp <- get.dp(stock, years)
     return(calc.discards(landings.constraint(stock=stock, sr=sr, landings=landings, dp=dp, years=years),years,dp))     
   }
)


# projecter(stock=FLStock, sr=FLSR, catch=FLQuant, ssb=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="FLQuant", landings="missing", ssb="FLQuant"),
   function(stock, sr, catch, ssb, years, ...){ 
     dp <- get.dp(stock, years)
  #   lc <- FLQuant(NA, dimnames=list(age="all", year=ac(years)))
     return(calc.discards(mixed.constraints(stock=stock, sr=sr, catch=catch, ssb=ssb, dp=dp, years=years),years,dp))
   }
)



# projecter(stock=FLStock, sr=FLSR, catch=FLQuant, landings=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="FLQuant", landings="FLQuant", ssb="missing"),
   function(stock, sr, catch, landings, years, ...){ 
     dp <- get.dp(stock, years)
     return(calc.discards(mixed.constraints(stock=stock, sr=sr, catch=catch, landings=landings, dp=dp, years=years),years,dp))
   }
)




# projecter(stock=FLStock, sr=FLSR, landings=FLQuant, ssb=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="missing", landings="FLQuant", ssb="FLQuant"),
   function(stock, sr, landings, ssb, years, ...){ 
     dp <- get.dp(stock, years)
     return(calc.discards(mixed.constraints(stock=stock, sr=sr, landings=landings, ssb=ssb, dp=dp, years=years),years,dp))
   }
)




# projecter(stock=FLStock, sr=FLSR, catch=FLQuant, landings=FLQuant, ssb=FLQuant) {{{
setMethod("projecter", signature(stock="FLStock", sr="FLSR", catch="FLQuant", landings="FLQuant", ssb="FLQuant"),
   function(stock, sr, catch, landings, ssb, years, ...){ 
     dp <- get.dp(stock, years)
     return(calc.discards(mixed.constraints(stock=stock, sr=sr, catch=catch, landings=landings, ssb=ssb, dp=dp, years=years),years,dp))
   }
)




