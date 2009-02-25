# classes - «Short one line description»
# classes

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: classes.R,v 1.6 2009/01/04 11:25:54 imosqueira Exp $

# sqliteFL class
setClass('sqliteFL', representation(db='character', name='character',     
  flrclass='character'))
setClass('sqliteFLQuant', representation('sqliteFL'))
setClass('sqliteFLComp', representation('sqliteFL'))
setClass('sqliteFLStock', representation('sqliteFLComp'))
