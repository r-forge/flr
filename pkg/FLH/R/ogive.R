# ogive - «Short one line description»
# ogive

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 24 Nov 2010 03:29:20 PM CET IM:

dnormal<-function(x,a1,sL,sR){
  pow<-function(a,b) a^b
 
  func<-function(x,a1,sL,sR){
    if (x < a1)
       return(pow(2.0,-((x-a1)/sL*(x-a1)/sL)))
    else
       return(pow(2.0,-((x-a1)/sR*(x-a1)/sR)))}
 
  sapply(x,func,a1,sL,sR)
  }
 
 logistic<-function(x,a50,ato95){
  pow<-function(a,b) a^b
 
  func<-function(x,a50,ato95){
     if ((a50-x)/ato95 > 5)
        return(0)
     if ((a50-x)/ato95 < -5)
        return(1)
 
     return(1.0/(1.0+pow(19.0,(a50-x)/ato95)))
     }
 
  sapply(x,func,a50,ato95)
  }



