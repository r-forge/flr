# SQLiteFL - «Short one line description»
# SQLiteFL

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: SQLiteFL.R,v 1.15 2009/01/21 12:21:33 imosqueira Exp $

# -----
library(SQLiteFL)
db <- tempfile()

data(ple4)

# sql
sql4  <- sql(ple4, db=db)

# summary
summary(sql4)

# dims
dims(sql4)

# catch.n
catch.n(sql4)
catch.n(sql4, year=1998)
catch.n(sql4, year=1998, age=2)

# FLComp methods

# -----
unlink(db)
