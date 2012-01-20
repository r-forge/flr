#linf,winf,a,b,k,t,amat
gundersonDygert=function(gsi)
  0.03 + 1.68*gsi

#http://icesjms.oxfordjournals.org/content/66/9/1978.full
#http://onlinelibrary.wiley.com/doi/10.1111/j.1467-2979.2009.00350.x/full


## Constant values
pauly1=function(winf,k,t=10)
   exp(-0.2107-0.0824*log(winf)+0.6757*log(k)+0,4627*log(t))

pauly2=function(linf,k,t=10)
   exp(-0,0066-0,279*log(linf)+0,6543*log(k)+0,4634*log(t)

hoenig=function(amax)
   4.22*amax

jensen=function(k)
   1.5*k

RickterEfanov=function(amat)
   1.52/(amat^0.72)-0.16 
  
PetersonWroblewski=function(wt)
   1.29*wt-0.25

Lorenzen=function(wt)
   3.00*wt-0.288

McGurk=function(wt)
   0.00526*wt-0.25

## variable
gislason=function(l,linf,k) 
   exp(0.55-1.61*log(l) + 1.44*log(linf) + log(k))

ChenWatanabe=function(age,k,t0=-0.1){
   m =k/(1-exp(-k*(age-t0)))

   tm =-(1/k)*log(1-exp(k*t0))+t0
   print(tm)
   bit=exp(-k*(tm-t0))
   
   a0=1-bit
   a1=k*bit
   a2=-0.5*k^2*bit
   age.=age>tm
   m[age.] =k/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
  
   return(m)}   
       