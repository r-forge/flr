# noise generator
# Ranta and Kaitala 2001 Proc. R. Soc.

# vt = B * vt-1 + s * sqrt(1 - B^2)

# s is normally distributed random variable with mean = 0, variance = 1
# B is the autocorrelation parameter

#spectra(.5*(1+noise(30,.8)*.3))

noise <- function(len,B=0,trun=NA){

    x <- rep(0, len+1) # going to hack off the first value at the end
    s <- rnorm(len,mean=0,sd=1)
    
    for(i in 1:len){
      x[i+1] <- B * x[i] + s[i] * sqrt(1 - B^2)
      if(!is.na(trun)){
        if (x[i+1] > (1-trun))  x[i+1] <- ( 1-trun)
        if (x[i+1] < (-1+trun)) x[i+1] <- (-1+trun)
          }
      }
    
    x<-x[-1]
    
    return(x)}

noise. <- function(s,len=length(s),B=0,trun=NA){
  
    x <- rep(0, len+1) # going to hack off the first value at the end
    
    for(i in 1:len){
      x[i+1] <- B * x[i] + s[i] * sqrt(1 - B^2)
      if(!is.na(trun)){
        if (x[i+1] > (1-trun))  x[i+1] <- ( 1-trun)
        if (x[i+1] < (-1+trun)) x[i+1] <- (-1+trun)
          }
      }
    
    x<-x[-1]
    
    return(x)}


#white <- noise(10000,B=0)
#red <- noise(100000,B=0.8)
#blue <- noise(1000,B=-0.8)
