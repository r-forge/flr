# FLStock - «Short one line description»
# FLStock

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 25 Feb 2009 00:14
# $Id$

# Reference:
# Notes:


# sql {{{
setMethod('sql', signature(object='FLStock'),
  function(object, db='SQLiteFL.db', name=as.list((match.call(call=sys.call(1))))$object)
  {
    # call .insert
    .Call('insertFLComp', db, as.character(name), object,
      names(getSlots(class(object)))[getSlots(class(object)) == 'FLQuant'])

    # return handler
    return(new('sqliteFLStock', db=db, name=as.character(name), flrclass=class(object)))
  }
) # }}}

# catch.n {{{
setMethod('catch.n', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'catch.n', ...)
  }
) # }}}

# catch.n<-(sqliteFLStock, numeric) {{{
setMethod('catch.n<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'catch.n', value, ...)
    return(object)
  }
)
# catch.n<-(sqliteFLStock, FLQuant)
setMethod('catch.n<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='catch.n', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}
