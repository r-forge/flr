# classes - «Short one line description»
# classes

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# sqliteFL class
setClass('sqliteFL', representation(db='character', name='character',     
  flrclass='character'))

# sqliteFLQuant
setClass('sqliteFLQuant', representation('sqliteFL'))

# sqliteFLComp
setClass('sqliteFLComp', representation('sqliteFL'))

# sqliteFLStock
setClass('sqliteFLStock', representation('sqliteFLComp'),
  validity=function(object) {
    if(object@flrclass != "FLStock")
      return("SQL object linked to is not of class 'FLStock'")
    return(TRUE)
  }
)

# sqliteFLlst
setClass('sqliteFLlst', representation('FLlst'),
  validity= function(object) {
    if(!all(lapply(object, is, 'sqliteFL') == TRUE))
      return("All elements must of class 'sqliteFL'")

    return(TRUE)
  }
)
