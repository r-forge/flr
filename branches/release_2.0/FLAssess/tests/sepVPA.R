# SepVPA test - this needs work

library(FLCore)
library(FLAssess)

# start test
setCon()
zz <- startTest("SepVPA.txt")
tagTest("SepVPA testing ...")

# Test all this...

data(ple4)
# replace 0s with 1s
ple4.test <- ple4
catch.n(ple4.test)[catch.n(ple4.test)==0] <- 1
# catch is odd in 1982 in pg; it's huge compared to biomass.
# This causes VPA to fail so need to reduce
catch.n(ple4.test)[,"1982"] <- 1400
#landings.n(ple4.test)[landings.n(ple4.test)==0] <- 1
stock.n(ple4.test)[] <- NA
harvest(ple4.test)[] <- NA

checkRun(control <- FLSepVPA.control(sep.age = 5))
checkRun(ple4.sepVPA <- SepVPA(ple4.test,control))

finishTest()

