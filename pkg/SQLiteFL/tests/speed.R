# speed - «Short one line description»
# speed

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 08 Jan 2009 16:02
# $Id$

# Reference:
# Notes:

# TODO Wed 07 Jan 2009 03:47:58 PM CET IM:

library(SQLiteFL)
db <- tempfile()

data(ple4)

res <- data.frame(iter=c(1, 5, 10, 25), save=NA, slot=NA)
# res <- data.frame(iter=c(1, 5, 10, 25, 50, 100, 250), save=NA, slot=NA)

for (i in res$iter)
{
  object <- propagate(ple4, i)
  res[res$iter==i, 'save'] <- system.time(sql4 <- sql(object, name='ple4', db=db))[3]
  res[res$iter==i, 'slot'] <- system.time(catch.n(sql4))[3]
}

# png(file='sqlspeed_with_index.png')
plot(res$iter, res$save, type='b', col='red')
lines(res$iter, res$slot, type='b', col='blue')
# dev.off()
# -----
unlink(db)
