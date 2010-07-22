# Tests for FLAssess class

library(FLCore)
library(FLAssess)

# start test
setCon()
zz <- startTest("project.txt")
tagTest("project testing ...")

# Test constructor
checkRun(projectControl())

# To test that the project runs without worrying yet about the numbers
data(ple4)
checkRun(ple4.stf <- stf(ple4, 3))

# without an SRR
checkRun(ple4.srr <- sr(as.FLSR(ple4, model='geomean')))
checkRun(control  <- projectControl(data.frame(year=c(2009:2011), val=rep(0.4,3), quantity=rep('f', 3))))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))
checkEqual(c(round(fbar(ple4.proj)[,ac(2009:2011)],1)), rep(0.4,3))

# with an SRR
checkRun(ple4.srr <- sr(as.FLSR(ple4, model='bevholt')))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))

# catch constraint
checkRun(control  <- projectControl(data.frame(year=c(2009:2011), val=rep(90000,3), quantity=rep('catch', 3))))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))

# ssb constraint
checkRun(control  <- projectControl(data.frame(year=c(2009:2011), val=c(rep(200000,2),0.5), quantity=c(rep('ssb',2),'f'))))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))

# mixed constraints
checkRun(ple4.stf <- stf(ple4, 4))
checkRun(control  <- projectControl(data.frame(year=c(2009:2012), val=c(0.5, 90000,200000,1), quantity=c('f','catch','ssb','f'))))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))

# landings and catch
checkRun(control  <- projectControl(data.frame(year=c(2009:2012), val=c(40000,40000,20000,20000), quantity=rep(c('landings','catch'),2))))
checkRun(ple4.proj<- project(ple4.stf, control, ple4.srr))

checkEqual(round(c(landings(ple4.proj)[,ac(c(2009,2011))]),0), c(40000,20000))
checkEqual(round(c(catch(ple4.proj)[,ac(c(2010,2012))]),0),    c(40000,20000))




finishTest()

