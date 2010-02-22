# Testing VPA - not the numbers, just that the thing doesn't fall over

library(FLCore)
library(FLAssess)

# start test
setCon()
zz <- startTest("VPA.txt")
tagTest("VPA testing ...")

# Test all this...

data(ple4)
# replace 0s with 1s
ple4.test <- ple4
catch.n(ple4.test)[catch.n(ple4.test)==0] <- 1
landings.n(ple4.test)[landings.n(ple4.test)==0] <- 1
stock.n(ple4.test)[] <- NA
harvest(ple4.test)[] <- NA

# Set Fs in final year in and in final ages of each year
ple4.1 <- ple4.test
harvest(ple4.1)[,"2001"] <- harvest(ple4)[,"2001"]
harvest(ple4.1)["10",] <- harvest(ple4)["10",]
checkRun(ple4.1.vpa <- VPA(ple4.1))
checkTrue(!any(is.na(stock.n(ple4.1.vpa))))
# Could do with better check here - i.e. check that not NAs

# Or give just Fs in final year and supply an Fratio
ple4.2 <- ple4.test
harvest(ple4.2)[,"2001"] <- harvest(ple4)[,"2001"]
checkRun(ple4.2.vpa <- VPA(ple4.2,fratio = 1))
checkTrue(!any(is.na(stock.n(ple4.2.vpa))))

# fit plusgroup tests





finishTest()
