# intro_FLR_loading_data - «Short one line description»
# intro_FLR_loading_data

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Mon 29 Mar 2010 07:55:37 PM CEST IM:

# From table to data.frame to FLQuant

# wide format
wid <- read.table(file='data_wide.dat', sep="\t", header=TRUE)

tabq <- FLQuant(as.matrix(wid)[,-1], dimnames=list(age=1:10, year=1990:1999), units='t')

# data.frame in FLQuant-friendly format
flq <- FLQuant(rnorm(200), dimnames=list(age=1:10, year=1990:1999), units='t')

# as exported by as.data.frame
# - column named 'data' for numbers in array
head(as.data.frame(flq))

# but file can be reduced to only columns with information
lon <- read.csv(file='data_long.dat', header=TRUE)

flq <- as.FLQuant(lon, units='t')

# data in wide format for FLStock
stk <- read.table('stock_data_wide.dat', header=TRUE, sep='\t')

# create a list
lst <- FLQuants()
# and loop through sections of stk to fill it
for (i in 3:4)
{
  df <- stk[,-i]
  names(df)[3] <- 'data'
  # with FLQuant objects
  lst[[names(stk)[i]]] <- as.FLQuant(df)
}
  
# call FLStock creator with list elements as arguments
sto <- FLStock(catch.n=lst[['catch.n']], catch.wt=lst[['catch.wt']])

# trick: use list as argument for do.call
sto <- do.call('FLStock', lst)

# functions exist for loading data in VPASuite, Adapt, ICA, MFCL and VPA2Box formats

# VPA Suite or LWT format

cod4 <- readFLStock("cod_2007/Cod347.idx", tyep="VPA")

summary(cod4)

# Landings and discards added together in this object, so need to compute manually

# Swap the landings and catch over
catch.n(cod4) <- landings.n(cod4)

# Read individual files for landings and discards
landings.n(cod4) <- readVPAFile(paste(input.dir,"Cod347cn_L.dat",sep="/"))
discards.n(cod4) <- readVPAFile(paste(input.dir,"Cod347cn_D.dat",sep="/"))

# Do the same for the weights
catch.wt(cod4) <- readVPAFile(paste(input.dir,"Cod347cw_T.dat",sep="/"))
landings.wt(cod4) <- readVPAFile(paste(input.dir,"Cod347cw_L.dat",sep="/"))
discards.wt(cod4) <- readVPAFile(paste(input.dir,"Cod347cw_D.dat",sep="/"))

# And the same for the totals
catch(cod4) <- readVPAFile(paste(input.dir,"Cod347la_T.dat",sep="/"))
landings(cod4) <- readVPAFile(paste(input.dir,"Cod347la_L.dat",sep="/"))
discards(cod4) <- readVPAFile(paste(input.dir,"Cod347la_D.dat",sep="/"))

# Ideally the total weight of landings should equal the sum of the numbers
# multiplied by the weights, so we should check consistency
computeLandings(cod4)
landings(cod4)

# Some small differences so we should recompute catch etc
landings(cod4) <- computeLandings(cod4)
discards(cod4) <- computeDiscards(cod4)
catch(cod4) <- computeCatch(cod4)

# Finally, set the fbar range and the plusgroup
# Take a look at the range
range(cod4)
# Now set the fbar range
range(cod4,c("minfbar","maxfbar"))<-c(2,4)
# Set the plusgroup
plusgroup<-7
cod4<-setPlusGroup(cod4,plusgroup,na.rm=TRUE)
summary(cod4)

# Read table accepts web addresses too.  This can be used to read in data
# from the internet
nao <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")

# set up dimnames of object (years, seasos, ...)
dnms <- list(quant="nao", year=1948:2009, season=1:12)

nao.flq <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")


