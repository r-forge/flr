###################################################
### chunk number 1: 
###################################################
require(FLEDA)
data(ple4)


###################################################
### chunk number 2: 
###################################################
# setting reference lines to white
trellis.par.set(reference.line=list(col="white"), strip.background=list(col="white"))


###################################################
### chunk number 3: 
###################################################
print(xyplot(data~year, data=catch(ple4), type=c("g","l")))


###################################################
### chunk number 4: 
###################################################
print(xyplot(data~year, groups=age, data=catch.n(ple4), type=c("g","l")))


###################################################
### chunk number 5: 
###################################################
print(xyplot(data~year|factor(age), data=catch.n(ple4), type=c("g","l"), scales=list(y=list(relation="free")), layout=c(2,5)))


###################################################
### chunk number 6: 
###################################################
print(xyplot(data~age|factor(year), data=catch.n(ple4), type=c("g","l")))


###################################################
### chunk number 7: 
###################################################
print(bubbles(age~year, data=catch.n(ple4), bub.scale=10))


###################################################
### chunk number 8: 
###################################################
print(xyplot(data~cohort, groups=age, data=FLCohort(catch.n(ple4)), type=c("g","l")))


###################################################
### chunk number 9: 
###################################################
print(xyplot(data~cohort|factor(age), data=FLCohort(catch.n(ple4)), type=c("g","l"), scales=list(y=list(relation="free")), layout=c(2,5)))


###################################################
### chunk number 10: 
###################################################
print(xyplot(data~age|factor(cohort), data=FLCohort(catch.n(ple4)), type=c("g","l")))


###################################################
### chunk number 11: 
###################################################
head(as.data.frame(ple4))


###################################################
### chunk number 12: 
###################################################
print(xyplot(data~year|slot, groups=age, data=ple4, scales=list(y=list(relation="free")), type=c("g","l")))


###################################################
### chunk number 13: 
###################################################
flqs <- FLQuants(R=rec(ple4), ssb=ssb(ple4), f=fbar(ple4), cactch=catch(ple4))
head(as.data.frame(flqs))
print(xyplot(data~year | qname, data=flqs, type=c("g", "l"), scales=list(y=list(relation="free"))))


