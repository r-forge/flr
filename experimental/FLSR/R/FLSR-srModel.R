# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT

# Reference:
# Notes: Updated to make generic

flagNormLog=FALSE

#### parameters for differebt SRRs
initialOps<-c("a",    "v,spr0=spr0",
              "a,b",  "s,v,spr0=spr0",
              "a,b",  "s,v,spr0=spr0",
              "a,b",  "s,v,spr0=spr0",
              "a,b",  "s,v,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0",
              "a,b,c=c","s,v,c=c,spr0=spr0")

initialOps<-t(array(initialOps,c(2,11),list(arg=c("ab","sv"),srr=c("mean","bevholt","ricker","cushing","segreg","shepherd","dersh","pellat","bevholtD","rickerD","shepherdD"))))

#### Calculates SSB as a function of SPR
sprFunc<-function(type,spr,a=NULL,b=NULL,c=NULL,d=NULL){
      # SSB as function of ssb/rec
      switch(type,
        "bevholt"  =a*(spr)-b,
        "ricker"   =log(a*spr)/b,
        "cushing"  =(1/(a*spr))^(1/(b-1)),
        "shepherd" =b*(a*spr-1)^(1/c),
        "segreg"   =ifelse(ssb <= b, a/(spr), 0),
        "mean"     =a/(spr),
        "dersh"    =ssb*a*(1-b*c*ssb)^c,
        "pellat"   =1/(a/ssb-a/ssb*(ssb/b)^c),
        "bevholtD" =NULL,
        "rickerdD" =NULL,
        "shepherdD"=NULL)}

#### SRRs
srrFunc<-function(type,ssb=NULL,spr=NULL,a=NULL,b=NULL,c=NULL,d=NULL){
    #recruits as function of ssb or ssb/red
    if (is.null(ssb) & !is.null(spr))
       ssb<-sprFunc(type,spr,a,b,c)

      switch(type,
          "bevholt"  =a*ssb/(b+ssb),
          "ricker"   =a*ssb*exp(-b*ssb),
          "cushing"  =a*ssb^b,
          "shepherd" =a*ssb/(1+(ssb/b)^c),
          "segreg"   =ifelse(ssb<=b,a*ssb,a*b),
          "mean"     =a,
          "dersh"    =a*(1-b*c*ssb)^(1/c),
          "pellat"   =a*(1-(ssb/b)^c),
          "bevholtD" =a*ssb/(b+ssb),
          "rickerD"  =a*ssb*exp(-b*ssb),
          "shepherdD"=a*ssb^2/(1+(ssb/b)^c))}

#### calls ssrFunc after tweaking alpha & beta
srr<-function(mdl,ssb,a,b,c=1,ca=0,cb=0,ndc=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   if (!is.null(covar)){
     a  <-a*(1+ca*covar)
     b  <-b*(1+cb*covar)}

   res<-srrFunc(mdl,ssb=ssb,,a=a,b=b,c=c)

   return(res)}

#### calls ssrFunc after conversion of steepness & virgin biomass to alpha & beta
srrSV<-function(mdl,ssb,s,v,c=1,ca=0,cb=0,ndc=0,spr0=0,covar=NULL){
   ssb<-ssb*(1+ndc)
   if (!is.null(covar)){
     s  <-s*(1+cs*covar)
     v  <-v*(1+cv*covar)}

   par<-ab(md,spr0,s=s,v=v,c=c)
   res<-srrFunc(mdl,ssb=ssb,,a=par["a"],b=par["b"],c=c)

   return(res)}

#### Loglikelihoods
loglFunc<-function(type){
    # Loglikelihoods
    res<-switch(type,
        "normlog"   ="sum(dnorm(rec, hat, sigma, TRUE), na.rm=TRUE)",
        "normal"    ="sum(dnorm(rec, hat, sigma, FALSE), na.rm=TRUE)",
        "lognorm"   ="loglAR1(0,  sigma,rec,hat)",
        "normar1"   ="loglAR1(rho,sigma,rec,hat)",
        "lognormar1"="loglAR1(rho,sigma,rec,hat)")

     if (is.null(res)) stop("has to be 1 of normlog, normal, lognorm, normar1 or lognormar1")
     return(res)}

#### Creates function call to SRR
modelFunc<-function(type,sv=FALSE,ca=FALSE,cb=FALSE,cs=FALSE,cv=FALSE,ndc=FALSE){

    if (!sv){
       res<-paste("srr(\"",type,"\",ssb,",sep="")
       res<-paste(res,initialOps[type,"ab"],sep="")}
    else{
       res<-paste("srrSV(\"",type,"\",ssb,",sep="")
       res<-paste(res,initialOps[type,"sv"],sep="")}
       
    #### Extra args
    argFlag<-c(ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc)
    argList<-c(ca=",ca=ca",cb=",cb=cb",cs=",cs=cs",cv=",cv=cv",ndc=",ndc=ndc")[argFlag]

    if (length(argList)>0)
       res<-paste(res,argList,sep="")
       
    res<-paste(res,")",sep="")

    return(res)}

#### Creates function for starting values
InitialVals<-function(mdl,sv=FALSE,ca=FALSE,cb=FALSE,cs=FALSE,cv=FALSE,ndc=FALSE){

    res<-c("foo<-structure(## initial parameter values\n",
           "\tfunction(rec, ssb){\n")

    if (sv)  res<-c(res,
                    "\t\ts<-\t0.75\n",
                    "\t\tv<-\tmean(ssb,na.rm=T)\n")

    if (ca)  res<-c(res,"\t\tca  <-0\n")
    if (cb)  res<-c(res,"\t\tcb  <-0\n")
    if (cs)  res<-c(res,"\t\tcs  <-0\n")
    if (cv)  res<-c(res,"\t\tcv  <-0\n")
    if (ndc) res<-c(res,"\t\tndc <-0\n")

    argFlag<-c(ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc)
    argList<-c(ca=",ca=ca",cb=",cb=cb",cs=",cs=cs",cv=",cv=cv",ndc=",ndc=ndc")

    res<-c(res,switch(mdl,
        "bevholt" =c("\t\ta <- max(quantile(c(rec), .75, na.rm=TRUE))\n",
          		  	   "\t\tb <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))\n",
                     "\t\tb<-a/b\n"),

        "ricker"  =c("\t\tx    <-ssb\n",
        			       "\t\ty    <-log(rec/ssb)\n",
                     "\t\tres  <-coefficients(lm(c(y)~c(x)))\n",
                     "\t\ta<-max(exp(res[1]))\n",
                     "\t\tb<--max(res[2])\n"),

        "cushing" =c("\t\ta <- mean(rec/ssb)\n",
          		       "\t\tb <- 1.0\n"),

        "shepherd"=c("\t\tc<-2\n",
                     "\t\ta<-quantile(c(rec/ssb),prob=0.75,na.rm=T)\n",
                     "\t\tb<-(quantile(c(rec),prob=0.75,na.rm=T)*quantile(c(ssb^(c-1)),prob=0.5,na.rm=T)/a)^(1/c)\n",

                     "\t\tnames(a)<-NULL\n",
                     "\t\tnames(b)<-NULL\n"),

       "segreg"   =c("\t\ta <- median(c(rec/ssb),na.rm=T)\n",
                     "\t\tb <- median(c(ssb)    ,na.rm=T)\n"),

        "mean"    =c("\t\ta<-mean(rec, na.rm=TRUE)\n"),


        "dersh"   =c("\t\ta<-mean(rec, na.rm=TRUE)\n",
               	     "\t\tb<-mean(rec, na.rm=TRUE)\n",
                     "\t\tc<-1\n"),

        "pellat"  =c("\t\ta<-mean(rec, na.rm=TRUE)\n",
                     "\t\tb<-mean(rec, na.rm=TRUE)\n"),

        "bevholtD"=c("\t\ta <- max(quantile(c(rec), .75, na.rm=TRUE))\n",
          		  	   "\t\tb <- max(quantile(c(rec)/c(ssb), .90, na.rm=TRUE))\n",
                     "\t\tb<-a/b\n"),

        "rickerD" =c("\t\tx    <-ssb\n",
        			       "\t\ty    <-log(rec/ssb)\n",
                     "\t\tres  <-coefficients(lm(c(y)~c(x)))\n\n",

                     "\t\ta<-max(exp(res[1]))\n",
                     "\t\tb<--max(res[2])\n"),

        "shepherdD"=c("\t\ta <-mean(rec/ssb,na.rm=T)\n",
                      "\t\tb <-mean(ssb,na.rm=T)/a\n",
                      "\t\tc <-1.0\n")))

    if (!any(argFlag)){
       res<-c(res,"\n\t\tres<-list(a=a,b=b)")
       res<-c(res,"\n\t\treturn(res)},\n")
       res<-c(res,"\n\t\tlower=c(0, 10e-8),")
       res<-c(res,"\n\t\tupper=rep(Inf, 2))")}
    else{
       res<-c(res,"\n\t\tlist(a=a,b=b",argList[argFlag],")")
       res<-c(res,"\n\t\treturn(res)},\n")
       n  <-table(argFlag)["TRUE"]
       tmp<-rep(",0",n)
       res<-c(res,"\n\t\tlower=c(0, 10e-8",tmp,"),")
       tmp<-rep(",1",n)
       res<-c(res,"\n\t\tupper=c(Inf, Inf",tmp,"))")}

    return(res)}

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
                       "\t\that  \t<-",modelFunc(mdl,sv,ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc),"\n",
                       "\t\that  \t<-",hat,"\n",
                       "\t\trec  \t<-",rec,"\n",
                       "\t\tsigma\t<-sigma(rec, hat)\n",
                       "\t\tres  \t<-",loglFunc(ll),"\n\n",
                       "\t\treturn(res)}\n",sep="",file="foo.txt")

    source("foo.txt")
    logl<-foo

    cat(InitialVals(mdl,sv=sv,ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc),file="foo.txt")
    source("foo.txt")
    initial<-foo

    cat(paste("foo<-formula(rec~",modelFunc(mdl,sv,ca=ca,cb=cb,cs=cs,cv=cv,ndc=ndc),")",sep=""),file="foo.txt")
    source("foo.txt")
    model<-foo

	  return(list(logl=logl, model=model, initial=initial))}
