# testDLL - «Short one line description»
# testDLL

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 08 Jan 2009 15:02
# $Id$

# Reference:
# Notes:

# TODO Mon 05 Jan 2009 04:58:37 PM CET IM:

# -----
library(SQLiteFL)
db <- tempfile()

data(ple4)

# insertFLComp
.Call('insertFLComp', db, 'ple4', ple4, names(getSlots(class(ple4)))[getSlots(class(ple4)) == 'FLQuant'])

# selectFLComp
ple4b <- .Call('selectFLComp', db, 'ple4')

all.equal(ple4, ple4b)

# selectSlotFLComp
catch_n <- .Call('selectSlotFLComp', db, 'ple4', 'catch.n')
all.equal(catch_n, catch.n(ple4))

.Call('sqliteVersion')


# FLComp methods

# -----
unlink(db)
