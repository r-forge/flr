# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# parLst {{{
parLst<-list(
  fox       =c("r","k"),
  schaefer  =c("r","k"),
  pellat    =c("r","k","p"),
  shepherd  =c("r","k","m"),
  gulland   =c("r","k"),
  fletcher  =c("k","msy","p"))
# }}}

# setParams {{{
setParams<-function(model="pellat",its=1)
  return(FLPar(NA,dimnames=list(params=c(parLst[[model]],"b0","q","sigma"),iter=its)))
# }}}

# defaultPar {{{
defaultPar<-function(object) {
   params(object)<-FLPar(NA,dimnames=list(params=c(parLst[[model(object)]],"b0","q","sigma"),iter=1:dims(object)$iter))

   unt<-NULL
   if ("r"     %in% dimnames(params(object))$params){
      params(object)["r",    ]<-0.5
      unt<-c(unt,"")}
   if ("k"     %in% dimnames(params(object))$params){
      params(object)["k",    ]<-mean(catch(object))*10
      unt<-c(unt,units(catch(object)))}
   if ("p"     %in% dimnames(params(object))$params){
      params(object)["p",    ]<-2
      unt<-c(unt,"")}
   if ("msy"   %in% dimnames(params(object))$params){
       params(object)["msy",  ]<-mean(catch(object))
       unt<-c(unt,units(catch(object)))}
   if ("b0"    %in% dimnames(params(object))$params){
       params(object)["b0",   ]<-1
       unt<-c(unt,"")}
   if ("m"     %in% dimnames(params(object))$params){
       params(object)["m",    ]<-0.5
       unt<-c(unt,"")}
   if ("q"     %in% dimnames(params(object))$params){
       params(object)["q",    ]<-mean(index(object))/(mean(catch(object))*10)
       unt<-c(unt,"")}
   if ("sigma" %in% dimnames(params(object))$params){
      params(object)["sigma",]<-0.3
      unt<-c(unt,"")}

   units(params(object))<-unt

   invisible(params(object))
   } # }}}

# getPar {{{
getPar<-function(params,nm){
   if (nm %in% dimnames(params)$params)
      return(c(params[nm,]))
   else
      return(rep(as.numeric(NA),length=dims(params)$iter))}
# }}}
