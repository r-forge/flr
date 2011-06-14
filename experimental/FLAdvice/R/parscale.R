
setGeneric("parscale", function(object, ...){
  standardGeneric("parscale")})


### Bevholt
#rec ~ a * ssb/(b + ssb)
#deriv(~a * ssb/(b + ssb),"ssb")
## a equals assymtopic recruitment
# a=mean(rec)
## a/b equals slope at origin
# b=a*mean(ssb/rec)
### Ricker
#rec ~ a * ssb * exp(-b * ssb)
##    .grad <- a * exp(-b * ssb) - (a * ssb) * (exp(-b * ssb) * b)
##if ssb=0
#    .grad <- a
### a=slope at origin
##    0 <- 1 - ssb * b
##    ssb=1/b at max rec
### Cushing
#rec ~ a * ssb^b
#deriv(~ a * ssb^b,"ssb")
# .grad <- a * (ssb^(b - 1) *b
#c(a=mean(rec/ssb),b=1)
### Shepherd
#rec ~ a * ssb/(1 + (ssb/b)^c)
### SegReg
#rec ~ FLQuant(ifelse(c(ssb) <= b, a * c(ssb), a * b), dimnames = dimnames(ssb))
#c(a=mean(rec/ssb),b=mean(ssb))

parscaleFn<-function(ssb,rec,model){
  switch(model,
         bevholt     =c(a=mean(rec,na.rm=T),      b=mean(ssb,na.rm=T)),
         ricker      =c(a=mean(rec/ssb,na.rm=T),  b=mean(ssb,na.rm=T)),
         segreg      =c(a=mean(rec/ssb,na.rm=T),  b=mean(ssb,na.rm=T)),
         shepherd    =c(a=mean(rec,na.rm=T),      b=mean(ssb,na.rm=T), c=1),
         cushing     =c(a=mean(rec/ssb,na.rm=T),  b=1),
         bevholtSV   =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
         rickerSV    =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
         segregSV    =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
         cushingSV   =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
         shepherdSV  =c(s=1,v=mean(ssb,na.rm=T),c=1,spr0=mean(ssb/rec,na.rm=T)))}

setMethod('parscale', signature(object='FLSR'),
  function(object)  parscaleFn(ssb(object),rec(object),SRModelName(model(object))))
