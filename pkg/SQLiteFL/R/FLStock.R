# FLStock - «Short one line description»
# FLStock

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 01 Oct 2009 23:31
# $Id$

# Reference:
# Notes:


# sql {{{
setMethod('sql', signature(object='FLStock'),
  function(object, db='SQLiteFL.db',
    name=as.list((match.call(call=sys.call(1))))$object)
  {
    # deal with name if [ or () was used
    if(is(name, 'call'))
      name <- format(name)

    # call .insert
    .Call('insertFLComp', db, as.character(name), object,
      names(getSlots(class(object)))[getSlots(class(object)) == 'FLQuant'])

    # return handler
    return(new('sqliteFLStock', db=db, name=as.character(name), flrclass=class(object)))
  }
) # }}}

# ACCESORS

# catch {{{
setMethod('catch', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'catch', ...)
  }
) # }}}

# catch<-(sqliteFLStock, numeric) {{{
setMethod('catch<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'catch', value, ...)
    return(object)
  }
)
# catch<-(sqliteFLStock, FLQuant)
setMethod('catch<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='catch', value=as.vector(value)),
      args))
    return(object)
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

# catch.wt {{{
setMethod('catch.wt', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'catch.wt', ...)
  }
) # }}}

# catch.wt<-(sqliteFLStock, numeric) {{{
setMethod('catch.wt<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'catch.wt', value, ...)
    return(object)
  }
)
# catch.wt<-(sqliteFLStock, FLQuant)
setMethod('catch.wt<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='catch.wt', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# landings {{{
setMethod('landings', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'landings', ...)
  }
) # }}}

# landings<-(sqliteFLStock, numeric) {{{
setMethod('landings<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'landings', value, ...)
    return(object)
  }
)
# landings<-(sqliteFLStock, FLQuant)
setMethod('landings<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='landings', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# landings.n {{{
setMethod('landings.n', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'landings.n', ...)
  }
) # }}}

# landings.n<-(sqliteFLStock, numeric) {{{
setMethod('landings.n<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'landings.n', value, ...)
    return(object)
  }
)
# landings.n<-(sqliteFLStock, FLQuant)
setMethod('landings.n<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='landings.n', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# landings.wt {{{
setMethod('landings.wt', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'landings.wt', ...)
  }
) # }}}

# landings.wt<-(sqliteFLStock, numeric) {{{
setMethod('landings.wt<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'landings.wt', value, ...)
    return(object)
  }
)
# landings.wt<-(sqliteFLStock, FLQuant)
setMethod('landings.wt<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='landings.wt', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# discards {{{
setMethod('discards', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'discards', ...)
  }
) # }}}

# discards<-(sqliteFLStock, numeric) {{{
setMethod('discards<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'discards', value, ...)
    return(object)
  }
)
# discards<-(sqliteFLStock, FLQuant)
setMethod('discards<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='discards', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# discards.n {{{
setMethod('discards.n', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'discards.n', ...)
  }
) # }}}

# discards.n<-(sqliteFLStock, numeric) {{{
setMethod('discards.n<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'discards.n', value, ...)
    return(object)
  }
)
# discards.n<-(sqliteFLStock, FLQuant)
setMethod('discards.n<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='discards.n', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# discards.wt {{{
setMethod('discards.wt', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'discards.wt', ...)
  }
) # }}}

# discards.wt<-(sqliteFLStock, numeric) {{{
setMethod('discards.wt<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'discards.wt', value, ...)
    return(object)
  }
)
# discards.wt<-(sqliteFLStock, FLQuant)
setMethod('discards.wt<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='discards.wt', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# stock {{{
setMethod('stock', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'stock', ...)
  }
) # }}}

# stock<-(sqliteFLStock, numeric) {{{
setMethod('stock<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'stock', value, ...)
    return(object)
  }
)
# stock<-(sqliteFLStock, FLQuant)
setMethod('stock<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='stock', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# stock.n {{{
setMethod('stock.n', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'stock.n', ...)
  }
) # }}}

# stock.n<-(sqliteFLStock, numeric) {{{
setMethod('stock.n<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'stock.n', value, ...)
    return(object)
  }
)
# stock.n<-(sqliteFLStock, FLQuant)
setMethod('stock.n<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='stock.n', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

# stock.wt {{{
setMethod('stock.wt', signature(object='sqliteFLStock'),
  function(object, ...)
  {
    selectFromFLComp(object, 'stock.wt', ...)
  }
) # }}}

# stock.wt<-(sqliteFLStock, numeric) {{{
setMethod('stock.wt<-', signature(object='sqliteFLStock', value='numeric'),
  function(object, ..., value)
  {
    updateFLComp(object, 'stock.wt', value, ...)
    return(object)
  }
)
# stock.wt<-(sqliteFLStock, FLQuant)
setMethod('stock.wt<-', signature(object='sqliteFLStock', value='FLQuant'),
  function(object, ..., value)
  {
    args <- list(...)
    if(length(args) == 0)
      args <- dimnames(value)
    do.call('updateFLComp', c(list(object=object, slot='stock.wt', value=as.vector(value)),
      args))
    return(object)
  }
) # }}}

