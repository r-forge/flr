# sp.R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, ICCAT
# $Id:  $

# sp {{{
setMethod('sp', signature(stock="FLBioDym", catch="missing", harvest="missing"),
  function(stock) {

    fox <-function(catch, params)
      params["r"]*catch*(1-log(catch)/log(params["K"]))
    schaefer <- function(catch, params)
      params["r"]*catch*(1-catch/params["K"])
    pellat <- function(catch, params)
      params["r"]/params["p"]*catch*(1-(catch/params["K"])^params["p"])
    shepherd <- function(catch,params)
      params["r"]*catch/(1+catch/params["K"])-params["m"]*catch
    gulland <- function(catch,params)
      params["r"]*catch*(params["K"]-catch)
    fletcher <- function(catch,params) {
      lambda <- (params["p"]^(params["p"]/(params["p"]-1)))/(params["p"]-1)
      lambda*msy*(catch/params["K"])-lambda*params["msy"]*(catch/params["K"])^params["p"]
    }

    res <- switch(model(stock),
           fox     =fox(catch(stock),params(stock)),
           schaefer=schaefer(catch(stock),params(stock)),
           gulland =gulland( catch(stock),params(stock)),
           fletcher=fletcher(catch(stock),params(stock)),
           pellat  =pellat(  catch(stock),params(stock)),
           shepherd=shepherd(catch(stock),params(stock)))

    return(res)
  }
)  # }}}
