# FLBiol - «Short one line description»
# SQLiteFL/R/FLBiol.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: FLStock.R 431 2010-02-05 13:36:30Z imosqueira $

# Reference:
# Notes:


# sql {{{
setMethod('sql', signature(object='FLBiol'),
  function(object, db='SQLiteFL.db',
    name=as.list((match.call(call=sys.call(1))))$object)
  {
    # deal with name if [ or () was used
    if(is(name, 'call'))
      name <- format(name)

    # call .insert
    .Call('insertFLComp', db, as.character(name), object,
      names(getSlots(class(object)))[getSlots(class(object)) == 'FLQuant'], "",
      PACKAGE='SQLiteFL')

    # return handler
    return(new('sqliteFLBiol', db=db, name=as.character(name), flrclass=class(object)))
  }
) # }}}

# ACCESORS

# n {{{
setMethod('n', signature(object='sqliteFLBiol'),
  function(object, ...)
  {
    selectFromFLComp(object, 'n', ...)
  }
) # }}}

# n<-(sqliteFLStock, numeric/FLQuant) {{{
setMethod('n<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'n', value, ...)
    return(object)
  }
)
# n<-(sqliteFLStock, FLQuant)
setMethod('n<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='n',
      value=as.vector(value)), args))
    return(object)
  }
) # }}}

# m {{{
setMethod('m', signature(object='sqliteFLBiol'),
  function(object, ...)
  {
    selectFromFLComp(object, 'm', ...)
  }
) # }}}

# m<-(sqliteFLStock, numeric/FLQuant) {{{
setMethod('m<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'm', value, ...)
    return(object)
  }
)
# m<-(sqliteFLStock, FLQuant)
setMethod('m<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='m',
      value=as.vector(value)), args))
    return(object)
  }
) # }}}

# wt {{{
setMethod('wt', signature(object='sqliteFLBiol'),
  function(object, ...)
  {
    selectFromFLComp(object, 'wt', ...)
  }
) # }}}

# wt<-(sqliteFLStock, numeric/FLQuant) {{{
setMethod('wt<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'wt', value, ...)
    return(object)
  }
)
# wt<-(sqliteFLStock, FLQuant)
setMethod('wt<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='wt',
      value=as.vector(value)), args))
    return(object)
  }
) # }}}

# fec {{{
setMethod('fec', signature(object='sqliteFLBiol'),
  function(object, ...)
  {
    selectFromFLComp(object, 'fec', ...)
  }
) # }}}

# fec<-(sqliteFLStock, numeric/FLQuant) {{{
setMethod('fec<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'fec', value, ...)
    return(object)
  }
)
# fec<-(sqliteFLStock, FLQuant)
setMethod('fec<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='fec',
      value=as.vector(value)), args))
    return(object)
  }
) # }}}

# spwn {{{
setMethod('spwn', signature(object='sqliteFLBiol'),
  function(object, ...)
  {
    selectFromFLComp(object, 'spwn', ...)
  }
) # }}}

# spwn<-(sqliteFLStock, numeric/FLQuant) {{{
setMethod('spwn<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'spwn', value, ...)
    return(object)
  }
)
# spwn<-(sqliteFLStock, FLQuant)
setMethod('spwn<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='spwn',
      value=as.vector(value)), args))
    return(object)
  }
) # }}}


# METHODS
