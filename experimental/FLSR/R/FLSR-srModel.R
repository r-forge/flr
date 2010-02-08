# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT

# Reference:
# Notes: Updated to make generic

flagNormLog=FALSE

srr<-c("bevholt","ricker","cushing","shepherd","segreg","mean","dersh","pellat","bevholtD","rickerD","shepherdD")

loglFunc<-function(type){
    # Loglikelihoods
    switch(type,
        "normlog"   ="sum(dnorm(rec, hat, sigma, TRUE), na.rm=TRUE)",
        "normal"    ="sum(dnorm(rec, hat, sigma, FALSE), na.rm=TRUE)",
        "lognorm"   ="loglAR1(0,  sigma,rec,hat)",
        "normar1"   ="loglAR1(rho,sigma,rec,hat)",
        "lognormar1"="loglAR1(rho,sigma,rec),hat)")}

srModel<- function(mdl="bevholt",ll="normlog",sv=FALSE,ca=FALSE,cb=FALSE,cs=FALSE,cv=FALSE,ndc=FALSE){

    #### Creates model structures

    ## parameters
    args<-unlist(initialOps[mdl,ifelse(sv,"sv","ab")])

    if (ll %in% c("normar1","lognormar1")) args<-c(args,"rho")

    if (gregexpr("log", ll)[[1]][1]>0) {
       rec="log(rec)"
       hat="log(hat)"
    }else{
       rec="rec"
       hat="hat"}

    ## log likelihood, assuming normal log.
    cat("foo<-function(",args,",rec,ssb){\n",
                       ifelse(sv,"\t\tsv2ab(s,v,spr0)\n",""),
                       "\t\that  \t<-",ac(modelFunc(mdl))[[3]],"\n",
                       "\t\that  \t<-",hat,"\n",
                       "\t\trec  \t<-",rec,"\n",
                       "\t\tsigma\t<-sigma(rec, hat)\n",
                       "\t\tres  \t\t<-",loglFunc(ll),"\n",
                       "\t\treturn(res)}\n",sep="",file="foo.txt")

    source("foo.txt")
    logl<-foo

    cat(InitialModel(mdl,sv=sv,ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc),file="foo.txt")
    source("foo.txt")
    initial<-foo

	  return(list(logl=logl, model=modelFunc(mdl,sv), initial=initial))}

.bevholt<-function(mdl,ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("bevholt",ssb=ssb,a=a,b=b)
   return(res)
   }

.ricker<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("ricker",ssb=ssb,a=a,b=b)
   return(res)
   }

.cushing<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("cushing",ssb=ssb,a=a,b=b)
   return(res)
   }

.shepherd<-function(ssb,a,b,c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("shepherd",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.segreg<-function(ssb,a,b,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("segreg",ssb=ssb,a=a,b=b)
   return(res)
   }

.dersch<-function(ssb,a,b,c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("dersch",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.mean<-function(ssb,a,ca=0){
   a  <-a*(1+ca*covar)

   res<-srrFunc("bevholt",ssb=ssb,a=a,b=b)
   return(res)
   }

.pellat<-function(ssb,a,b,c=c,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   a  <-a*(1+ca*covar)
   b  <-b*(1+cb*covar)

   res<-srrFunc("pellat",ssb=ssb,a=a,b=b,c=c)
   return(res)
   }

.bevholtSV<-function(ssb,spr0,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("bevholt",spr0,s=s,v=v)
   res<-srrFunc("bevholt",ssb=ssb,a=par["a"],b=par["b"])
   
   return(res)}

.rickerSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("ricker",spr0,s=s,v=v)
   res<-srrFunc("ricker",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.cushingSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("cushing",spr0,s=s,v=v)
   res<-srrFunc("cushing",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.shepherdSV<-function(ssb,s,v,c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("shepherd",spr0,s=s,v=v)
   res<-srrFunc("shepherd",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}

.segregSV<-function(ssb,s,v,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("segreg",spr0,s=s,v=v)
   res<-srrFunc("segreg",ssb=ssb,,a=par["a"],b=par["b"])
   
   return(res)}

.derschSV<-function(ssb,s,v,c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("dersch",spr0,s=s,v=v)
   res<-srrFunc("dersch",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}

.meanSV<-function(ssb,v,cs=0){
   v  <-v*(1+cs*covar)

   par<-ab("mean",spr0,v=v)
   res<-srrFunc("bevholt",ssb=ssb,,a=par["a"])
   
   return(res)}

.pellatSV<-function(ssb,s,v,c=c,cs=0,cv=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   s  <-s*(1+cs*covar)
   v  <-v*(1+cv*covar)

   par<-ab("pellat",spr0,s=s,v=v)
   res<-srrFunc("pellat",ssb=ssb,,a=par["a"],b=par["b"],c=c)
   
   return(res)}