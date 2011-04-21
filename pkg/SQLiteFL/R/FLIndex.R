# FLBiol - «Short one line description»
# SQLiteFL/R/FLBiol.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: FLStock.R 431 2010-02-05 13:36:30Z imosqueira $

# Reference:
# Notes:


# sql {{{
setMethod('sql', signature(object='FLIndex'),
  function(object, db='SQLiteFL.db',
    name=as.list((match.call(call=sys.call(1))))$object)
  {
    # deal with name if [ or () was used
    if(is(name, 'call'))
      name <- format(name)

    # call .insert
    .Call('insertFLComp', db, as.character(name), object,
      names(getSlots(class(object)))[getSlots(class(object)) == 'FLQuant'],
      c("type", "distribution"), PACKAGE='SQLiteFL')

    # return handler
    return(new('sqliteFLIndex', db=db, name=as.character(name), flrclass=class(object)))
  }
) # }}}

# ACCESORS

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


# METHODS
