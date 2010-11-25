# ogive - «Short one line description»
# ogive

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 24 Nov 2010 03:29:20 PM CET IM:

#
pow<-function(a,b) a^b

# dnormal(x, a1, sL, sR)
dnormal <- function(x,a1,sL,sR)
{
  func <- function(x,a1,sL,sR)
  {
    if (x < a1)
       return(pow(2.0,-((x-a1)/sL*(x-a1)/sL)))
    else
       return(pow(2.0,-((x-a1)/sR*(x-a1)/sR)))
  }
  sapply(x,func,a1,sL,sR)
}

# dnormal.capped(x, a, sL, sR, amax=1.0)
dnormal.capped <- function(x,a,sL,sR,amax=1.0) {
  func<-function(x,a,sL,sR,amax) {
    if (x < a)
       return(amax*pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else
       return(amax*pow(2.0,-((x-a)/sR*(x-a)/sR)))
  }
  sapply(x,func,a,sL,sR,amax)
}

# dnormal.plateau(x, a1, a2, amax, sL, sR)
dnormal.plateau <- function(x, a1, a2, amax, sL, sR) {
  if (x<=a1) 
     return(amax*2^-((x-a1)/sL)^2)
  else if (a1<x & x<=(a1+a2))
     return(amax*2^-((x-a1)/sL)^2)
  else
     return(amax*2^-((x-(a1+a2))/sR)^2)
}

# dnornmal.coleraine(x, a, b, c)
dnormal.coleraine <- function(x, a, b, c) {
  if (x<a)
      return(exp(-(x-a)^2/b^2))
  else
      return((-(x-a)^2/c^2))
}

# logistic(x, a50, ato95)
logistic <- function(x, a50, ato95)
{
  func <- function(x,a50,ato95)
  {
    if ((a50-x)/ato95 > 5)
      return(0)
    if ((a50-x)/ato95 < -5)
      return(1)
    return(1.0/(1.0+pow(19.0,(a50-x)/ato95)))
  }
  sapply(x,func,a50,ato95)
}

# logistic.double(x, a50, ato95, b50, bto95, amax=1.0)
logistic.double <- function(x, a50, ato95, b50, bto95, amax=1.0)
{
  func <- function(x,a50,ato95,b50,bto95,amax)
  {
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50)) return(amax) else return(0)
    } else if (ato95 < 0.02) {
      p = a50
      funcMax = 1+pow(19.0,(p-(a50+b50))/bto95)
      return(amax * funcMax * (1+pow(19.0,(x-(a50+b50))/bto95)))
    } else if (bto95 < 0.02) {
      p = a50+b50
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * (1+pow(19.0,(a50-x)/ato95)))
    } else {
      p = (a50 * bto95 + ato95 * (a50 + b50)) / (ato95 + bto95)
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * min(1/(1+pow(19.0,(a50-x)/ato95)),
        1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    }
  }

  sapply(x,func,a50,ato95,b50,bto95,amax)
}

# logistic.product
logistic.product <- function(x,a50,ato95,b50,bto95,amax=1.0)
{
  func <- function(x,a50,ato95,b50,bto95,amax)
  {
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50))
        return(amax)
      else
        return(0)
    } else if (ato95 < 0.02) {
        funcMax = 1+pow(19.0,(-b50)/bto95)
        return(amax * funcMax * (1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    } else if (bto95 < 0.02) {
        funcMax = 1+pow(19.0,(-b50)/ato95)
        return(amax * funcMax * (1/(1+pow(19.0,(a50-x)/ato95))))
    } else {
        funcMax = 0
        for (i in 0:100) {
          tempvar = a50 - ato95 + i * (b50 + bto95 + ato95) / 100
          funcMax = max(funcMax, (1+pow(19.0,(a50-tempvar)/ato95))*
            (1+pow(19.0,(tempvar-(a50+b50))/bto95)))
        }
        return(amax * funcMax * (1/((1+pow(19.0,(a50-x)/ato95))
          * (1+pow(19.0,(x-(a50+b50))/bto95)))))
     }
  }    
  sapply(x,func,a50,ato95,b50,bto95,amax)
}

# richards
richards <- function(x, a50, ato95, sigma) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)

  return((1/(1+19^(alpha-x)/beta))^1/sigma)
}

# richards.capped
richards.capped <- function(x, a50, ato95, sigma, amax) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)

  return((amax/(1+19^(alpha-x)/beta))^1/sigma)
}

# seldnc
seldnc <- function(x,a,b,c) {
  d <- rep(b,length(x))
  d[x>a] <- c
  return(exp(-(x-a)^2/d^2))
}
