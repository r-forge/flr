# sp.R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT
# $Id:  $


setMethod('sp', signature(stock="FLBioDym"),
  function(stock)
    sp(stock,stock(stock)))
    
setMethod('sp', signature(stock="FLBioDym",catch="FLQuant"),
  function(stock,catch,biomass=catch) {

    fox <-function(biomass, params)
          params["r"]*biomass*(1-log(biomass)/log(params["K"]))
    schaefer <- function(biomass, params)
          params["r"]*biomass*(1-biomass/params["K"])
#     pellat <- function(biomass, params)
#           params["r"]/params["p"]*biomass*(1-(biomass/params["K"])^params["p"])
     pellat <- function(biomass, params){
           a=sweep(biomass,6,params["r"]/params["p"],"*")
           b=sweep(biomass,6,params["K"],"/")
           c=sweep(b,      6,params["p"],"^")
           a*(1-c)}
    shepherd <- function(biomass,params)
          params["r"]*biomass/(1+biomass/params["K"])-params["m"]*biomass
    gulland <- function(biomass,params)
          params["r"]*biomass*(params["K"]-biomass)
    fletcher <- function(biomass,params) {
          lambda <- (params["p"]^(params["p"]/(params["p"]-1)))/(params["p"]-1)
          lambda*msy*(biomass/params["K"])-lambda*params["msy"]*(biomass/params["K"])^params["p"]}

    res <- switch(model(stock),
           fox     =fox(     biomass,params(stock)),
           schaefer=schaefer(biomass,params(stock)),
           gulland =gulland( biomass,params(stock)),
           fletcher=fletcher(biomass,params(stock)),
           pellat  =pellat(  biomass,params(stock)),
           shepherd=shepherd(biomass,params(stock)))

    return(res)}) 
