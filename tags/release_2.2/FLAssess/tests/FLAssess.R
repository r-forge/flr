# Tests for FLAssess class

library(FLCore)
library(FLAssess)

# start test
setCon()
zz <- startTest("FLAssess.txt")
tagTest("FLAssess testing ...")

# Test constructor
checkRun(new('FLAssess'))
# There is no public constructor.  FLAssess objects get created after an assessment.

# To test - there is no test FLAssess object so use a VPA object instead
data(ple4)
checkRun(ple4.vpa <- VPA(ple4))

# Just checking these methods don't crash
# summary
checkRun(summary(ple4.vpa))
# plot
checkRun(plot(ple4.vpa))

# merge - should take the harvest and stock.n slots from the Assess object and put them in the stock 
ple4.temp <- ple4
harvest(ple4.temp)[,ac(1999:2001)] <- NA
stock.n(ple4.temp)[,ac(1999:2001)] <- NA
# check harvest and stock.n match
checkRun(test <- merge(ple4.temp,ple4.vpa))
checkEqual(harvest(test),harvest(ple4.vpa))
checkEqual(stock.n(test),stock.n(ple4.vpa))

# similarly check that + behaves
# + (stock, assess)
checkRun(test <- ple4.temp + ple4.vpa)
checkEqual(harvest(test),harvest(ple4.vpa))
checkEqual(stock.n(test),stock.n(ple4.vpa))

# + (assess, stock)
checkRun(test <-ple4.vpa + ple4.temp)
checkEqual(harvest(test),harvest(ple4.vpa))
checkEqual(stock.n(test),stock.n(ple4.vpa))

finishTest()
# harvest (on an FLBiol)
