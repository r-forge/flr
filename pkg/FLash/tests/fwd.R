library(FLCore)
library(FLash)
library(FLAdvice)
library(FLBioDym)

## FLBRP with simulated equilibrium object
obj1 =gislaSim(FLPar(linf=100))

## FLStock with dynamics of obj1
obj2=as(obj1,"FLStock")

obj3=fwd(obj1,maxF=4)

obj4=fwd(obj2,sr=obj1,f=FLQuant(.5,dimnames=list(year=80:101)))



plot(FLStocks("as"=obj2,"fwd"=obj3))

## Run an MSE with biomass dynamic model
obj3=mseFLBioDym(obj3, start=80, sr=obj1, CV = 0.3) 

## 
ctrl =FLQuant(.5,dimnames=list(year=80:101))

plot(fwd(obj3,ctrl,sr=obj1))

plot(fwd(obj2,ctrl*400,sr=obj1,quantity="catch"))

ctrls=FLQuants("f"=FLQuant(0.5,dimnames=list(year=80:101)),
               "f"=FLQuant(1.0,dimnames=list(year=80:101)),
               "f"=FLQuant(1.5,dimnames=list(year=80:101)),
               "f"=FLQuant(2.0,dimnames=list(year=80:101)),
               "f"=FLQuant(2.5,dimnames=list(year=80:101)),
               "f"=FLQuant(3.5,dimnames=list(year=80:101)))

obj4=fwd(obj3,ctrls,sr=obj1)

plot(fwd(obj2,ctrls,sr=obj))
