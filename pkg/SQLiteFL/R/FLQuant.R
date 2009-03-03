# FLQuant - «Short one line description»
# SQLiteFL/R/FLQuant

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 02 Jan 2009 13:52
# $Id$

# Reference:
# Notes:

# TODO Thu 06 Mar 2008 05:28:53 PM CET IM:

# sqliteFL
setMethod('sql', signature(object='FLQuant'),
  function(object, db='SQLiteFL.db', name=as.list((match.call(call=sys.call(1))))$object)
  {
    # call .insert
    .Call('insertFLQ', name, object, db)

    # return handler
    return(new('sqliteFLQuant', data=paste(name, 'data', sep='_'),
      meta=paste(name, 'paste', sep='_')))
  }
)

