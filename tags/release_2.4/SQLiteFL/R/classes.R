# classes - «Short one line description»
# classes

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: classes.R 913 2011-03-22 10:20:01Z imosqueira $

# sqliteFL class
setClass('sqliteFL', representation(db='character', name='character',     
  flrclass='character'))

# sqliteFLQuant
setClass('sqliteFLQuant', representation('sqliteFL'))

# sqliteFLComp
setClass('sqliteFLComp', representation('sqliteFL'))

# sqliteFLStock {{{
setClass('sqliteFLStock', representation('sqliteFLComp'),
  validity=function(object) {
    if(object@flrclass != "FLStock")
      return("SQL object linked to is not of class 'FLStock'")
    return(TRUE)
  }
) # }}}

# sqliteFLBiol {{{
setClass('sqliteFLBiol', representation('sqliteFLComp'),
  validity=function(object) {
    if(object@flrclass != "FLBiol")
      return("SQL object linked to is not of class 'FLBiol'")
    return(TRUE)
  }
) # }}}

# sqliteFLIndex {{{
setClass('sqliteFLIndex', representation('sqliteFLComp'),
  validity=function(object) {
    if(object@flrclass != "FLIndex")
      return("SQL object linked to is not of class 'FLIndex'")
    return(TRUE)
  }
) # }}}

# sqliteFLlst {{{
setClass('sqliteFLlst', representation('FLlst'),
  validity= function(object) {
    if(!all(lapply(object, is, 'sqliteFL') == TRUE))
      return("All elements must of class 'sqliteFL'")

    return(TRUE)
  }
) # }}}
