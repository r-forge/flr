# SQLiteFL - «Short one line description»
# SQLiteFL/tests/SQLiteFL.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# -----
library(SQLiteFL)
db <- tempfile()

# FLStock
data(ple4)

# sql
sql4  <- sql(ple4, db=db)
sql4  <- sql(ple4[,1], db=db)
sql4  <- sql(iter(ple4,1), db=db)

# summary
summary(sql4)

# dims
dims(sql4)

# catch.n
catch.n(sql4)
catch.n(sql4, year=1998)
catch.n(sql4, year=1998, age=2)

# substitution
catch(sql4, year=1957) <- 10
catch(sql4, year=1957:1960) <- 10
catch(sql4, year=1957:1960) <- 1:4

# How to get this to work?
# catch(sql4) <- 1:52

catch(sql4) <- catch(ple4)

# FLBiol
bio4 <- as.FLBiol(ple4)

sqb4 <- sql(bio4)

n(sqb4)
wt(sqb4)

# FLComp methods

# FLIndex
pli4 <- as(ple4, 'FLIndex')

sql(pli4)

# -----
unlink(db)
