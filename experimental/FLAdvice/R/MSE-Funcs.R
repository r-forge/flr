#### Functions #################################################################
#### biological correlations and processes
## Von Bertalannfy growth function
growth=function(t,Linf,k,t0=0,a=0.001,b=3) a*(Linf*(1-exp(-k*(t-t0))))^b

## Natural Mortality as a function of Linf
M     =function(L,Linf,M1=0.1,h=1.71,n=-1.66,i=0.8) M1+h*Linf^i*L^n

## Maturity ogive
## Age at 50% maturity
mat50 =function(M,k) log(((3*k+M)/M)/k)

## logistic
logistic=function(x,a50,ato95) 1.0/(1.0+19.0^((a50-x)/ato95))

matOgive=function(x,a50,ato95) sapply(x,logistic,a50,a50+ato95)

# dnormal(x, a1, sL, sR) {{{
dnormal <- function(x,a1,sL,sR){
  pow<-function(a,b) a^b

  func <- function(x,a1,sL,sR){
    if (x < a1)
       return(pow(2.0,-((x-a1)/sL*(x-a1)/sL)))
    else
       return(pow(2.0,-((x-a1)/sR*(x-a1)/sR)))}

  sapply(x,func,a1,sL,sR)}
################################################################################

#### Life History Generator ####################################################
lhGen=function(k,Linf,steepness=0.75,vbiomass=1e3,sel=c(a=1,sL=1,sR=1),m=NULL,matPerc50=NULL,mat95=3,sr="bevholt",age=1:75,...){

   ## Dimensions, age by year range
   dms=list(age=age,year=1)

   ## Biological processes
   wts       =FLQuant(growth(age,Linf,k)                          ,dimnames=dms)
   if (is.null(m))
     m       =FLQuant(M(c(1000*growth(age+0.5,Linf,k))^(1/3),Linf),dimnames=dms)
   if (is.null(matPerc50))
     matPerc50=mat50(c(mean(m)),k)
   mat        =FLQuant(logistic(age,matPerc50,matPerc50+mat95)     ,dimnames=dms)
   selPattern =FLQuant(dnormal(age,(matPerc50+mat95)*sel["a"],
                                     (matPerc50+mat95)*sel["sL"],
                                     (matPerc50+mat95)*sel["sR"]),    dimnames=dms)

   ## create a FLBRP object to calculate expected equilibrium values and ref pts
   res=FLBRP(stock.wt       =wts,
             landings.wt    =wts,
             discards.wt    =wts,
             bycatch.wt     =wts,
             m              =m,
             mat            =mat,
             landings.sel   =FLQuant(selPattern,dimnames=dms),
             discards.sel   =FLQuant(0,         dimnames=dms),
             bycatch.harvest=FLQuant(0,         dimnames=dms),
             harvest.spwn   =FLQuant(0,         dimnames=dms),
             m.spwn         =FLQuant(0,         dimnames=dms),
             availability   =FLQuant(1,         dimnames=dms))

   ## i.e. FApex
   range(res,c("minfbar","maxfbar"))[]<-as.integer((matPerc50+mat95)*sel["a"])

   ## Stock recruitment realationship
   model(res) =do.call(sr,list())$model
   params(res)=FLPar(abPars(sr,spr0=spr0(res),s=steepness,v=vbiomass))

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))])
     slot(res, slt)<-args[[slt]]

   return(brp(res))}
################################################################################

#### Indicators ################################################################
mnLenCatch<-function(object,a=0.001,b=3)
    apply((catch.wt(object)/a)^(1/b)*catch.n(object),c(2,6),sum)/
    apply(catch.n(object),c(2,6),sum)

mLenStock<-function(object,a=0.001,b=3)
    apply((stock.wt(object)/a)^(1/b)*stock.n(object),c(2,6),sum)/
    apply(stock.n(object),c(2,6),sum)

w2z <-function(object,Linf,Lc,k,a=0.001,b=3){
    object<-(object/a)^(1/b)
    k*(Linf-object)/(object-Lc)}
l2z <-function(object,Linf,Lc,k){
    k*(Linf-object)/(object-Lc)}
ZStock <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzStock(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}
ZCatch <-function(object,Linf,Lc,k,a=0.001,b=3){
    mnSz<-mnSzCatch(object,a,b); k*(Linf-mnSz)/(mnSz-Lc)}
################################################################################

