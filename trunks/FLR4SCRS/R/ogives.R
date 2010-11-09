#pow:               a,b
#logistic:          a50,ato95
#logistic.double:   a50,ato95,b50,bto95,amax=1.0
#logistic.product:  a50,ato95,b50,bto95,amax=1.0
#richards:          a50, ato95, sigma
#richards.capped:   a50, ato95, sigma, amax
#dnormal.capped:    a,sL,sR,amax=1.0
#dnormal:           a,sL,sR
#dnormal.plateau:   a1,a2,amax,sL,sR
#dnormal.coleraine: a,b,c
#seldnc:            a,b,c

parNms<-list("logistic"         =c("a50","ato95"),
             "logistic.double"  =c("a50","ato95","b50","bto95","amax"),
             "logistic.product" =c("a50","ato95","b50","bto95","amax"),
             "richards"         =c("a50","ato95","sigma"),
             "richards.capped"  =c("a50","ato95","sigma","amax"),
             "dnormal.capped"   =c("a","sL","sR","amax"),
             "dnormal"          =c("a","sL","sR"),
             "dnormal.plateau"  =c("a1","a2","amax","sL","sR"),
             "dnormal.coleraine"=c("a","b","c"),
             "seldnc"           =c("a","b","c"))

checkParams<-function(x,model){

 if (!(model %in% names(parNms))) stop(paste("Model",model,"not recognised"))
 if (all(dimnames(params)$params %in% parNms[[model]])) return(TRUE) else return(FALSE)}

pow<-function(a,b) a^b

logistic<-function(x,a50,ato95){
    #return(c/(1+19^((a-x)/b)))
  pow<-function(a,b) a^b

  func<-function(x,a50,ato95){
    if ((a50-x)/ato95 > 5)
      return(0)
    if ((a50-x)/ato95 < -5)
      return(1)

    return(1.0/(1.0+pow(19.0,(a50-x)/ato95)))}

  sapply(x,func,a50,ato95)}

logistic.double<-function(x,a50,ato95,b50,bto95,amax=1.0){
  # min(amax/(1+19^(a50-x)/ato95),amax/(1+19^(x-(a50+b50))/bto95))*
  #          (1+19^(a50-(a50*bto95+ato95*(a50+b50))/(ato95+bto95))/ato95)
  #}

  func<-function(x,a50,ato95,b50,bto95,amax){
    if (ato95 < 0.02 && bto95 < 0.02){
      if (a50 <= x && x <= (a50+b50)) return(amax) else return(0)
    } else if (ato95 < 0.02){
      p = a50
      funcMax = 1+pow(19.0,(p-(a50+b50))/bto95)
      return(amax * funcMax * (1+pow(19.0,(x-(a50+b50))/bto95)))
    } else if (bto95 < 0.02){
      p = a50+b50
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * (1+pow(19.0,(a50-x)/ato95)))
    } else {
      p = (a50 * bto95 + ato95 * (a50 + b50)) / (ato95 + bto95)
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * min(1/(1+pow(19.0,(a50-x)/ato95)), 1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    }}

  sapply(x,func,a50,ato95,b50,bto95,amax)}

logistic.product<-function(x,a50,ato95,b50,bto95,amax=1.0){
  #tmp1<-(1+19^(a50-x)/ato95)
  #tmp2<-(1+19^(x-(a50+b50))/bto95)
  
  #return(amax/(tmp1*tmp2)/max(1/tmp1,tmp2))

  pow<-function(a,b) a^b

  func<-function(x,a50,ato95,b50,bto95,amax){
      if (ato95 < 0.02 && bto95 < 0.02){
        if (a50 <= x && x <= (a50+b50)) return(amax) else return(0)
      } else if (ato95 < 0.02){
        funcMax = 1+pow(19.0,(-b50)/bto95)
        return(amax * funcMax * (1/(1+pow(19.0,(x-(a50+b50))/bto95))))
      } else if (bto95 < 0.02){
        funcMax = 1+pow(19.0,(-b50)/ato95)
        return(amax * funcMax * (1/(1+pow(19.0,(a50-x)/ato95))))
      } else {
        funcMax = 0
        for (i in 0:100) {
          tempvar = a50 - ato95 + i * (b50 + bto95 + ato95) / 100
          funcMax = max(funcMax, (1+pow(19.0,(a50-tempvar)/ato95))*(1+pow(19.0,(tempvar-(a50+b50))/bto95)))
        }
        return(amax * funcMax * (1/((1+pow(19.0,(a50-x)/ato95)) * (1+pow(19.0,(x-(a50+b50))/bto95)))))
        }}
        
    sapply(x,func,a50,ato95,b50,bto95,amax)}

richards<-function(x, a50, ato95, sigma){
  beta <-ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha<-a50+beta*log(2^sigma-1)/log(19)

  (1/(1+19^(alpha-x)/beta))^1/sigma}

richards.capped<-function(x, a50, ato95, sigma, amax){
  beta <-ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha<-a50+beta*log(2^sigma-1)/log(19)

  (amax/(1+19^(alpha-x)/beta))^1/sigma}

dnormal.capped<-function(x,a,sL,sR,amax=1.0){
  #return((params[4]*2)^-((x-c)/ifelse(x>c,b,a))^2)
  pow<-function(a,b) a^b
  
  func<-function(x,a,sL,sR,amax){
    if (x < a)
       return(amax*pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else
       return(amax*pow(2.0,-((x-a)/sR*(x-a)/sR)))}

  sapply(x,func,a,sL,sR,amax)}

dnormal<-function(x,a,sL,sR){
#  if (x<=a)
#    return(2^-((x-a)/sL)^2)
#  else
#    return(2^-((x-a)/sR)^2)

  pow<-function(a,b) a^b

  func<-function(x,a,sL,sR){
    if (x < a)
       return(pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else
       return(pow(2.0,-((x-a)/sR*(x-a)/sR)))}

  sapply(x,func,a,sL,sR)
  }

dnormal.plateau<-function(x,a1,a2,amax,sL,sR){
  if (x<=a1)
     amax*2^-((x-a1)/sL)^2
  else if (a1<x & x<=(a1+a2))
     amax*2^-((x-a1)/sL)^2
  else
     amax*2^-((x-(a1+a2))/sR)^2
  }

dnormal.coleraine<-function(x,a,b,c){
  if (age<a)
      res<-exp(-(age-a)^2/b^2)
  else
      res<-exp(-(age-a)^2/c^2)
  }

#seldnc<-function(age,a,b,c) if (age<a) res<-exp(-(age-a)^2/b^2) else res<-exp(-(age-a)^2/c^2)

seldnc<-function(x,a,b,c) {
   d       <-rep(b,length(x))
   d[x>a]<-c
   return(exp(-(x-a)^2/d^2))}


