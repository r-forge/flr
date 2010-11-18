###################################################
### chunk number 1: 
###################################################
#line 35 "fleda.Rnw"
require(FLEDA)
data(ple4)
data(ple4sex)
data(ple4.index)
data(ple4.indices)


###################################################
### chunk number 2: 
###################################################
#line 46 "fleda.Rnw"
# setting reference lines to white
trellis.par.set(reference.line=list(col="white"), strip.background=list(col="white"))


###################################################
### chunk number 3: 
###################################################
#line 64 "fleda.Rnw"
apply(catch(ple4sex),3,summary)


###################################################
### chunk number 4: 
###################################################
#line 76 "fleda.Rnw"
summary(catch(ple4sex))


###################################################
### chunk number 5: 
###################################################
#line 88 "fleda.Rnw"
summary(apply(catch(ple4sex), 2, sum))


###################################################
### chunk number 6: 
###################################################
#line 100 "fleda.Rnw"
summary(unitSums(catch(ple4sex)))


###################################################
### chunk number 7: 
###################################################
#line 115 "fleda.Rnw"
# fine tune xyplot
ttl <- list(label="Catch trends by sex for plaice in IV", cex=1)
yttl <- list(label=units(ple4sex@catch), cex=0.7)
xttl <- list(cex=0.7)
stripttl <- list(cex=0.7)
ax <- list(cex=0.7)
# plot
print(xyplot(data~year|unit, data=ple4sex@catch, type=c("g", "l"), main=ttl, ylab=yttl, xlab=xttl, par.strip.text=stripttl, scales=ax))


###################################################
### chunk number 8: 
###################################################
#line 138 "fleda.Rnw"
print(xyplot(data~year, data=ple4sex@catch, groups=unit, type=c("g", "l"), main="catch trends by sex for plaice in IV", ylab=units(ple4sex@catch)))


###################################################
### chunk number 9: 
###################################################
#line 152 "fleda.Rnw"
# define the panel function to include a loess fit
pfun <- function(x,y, ...){
	panel.xyplot(x,y, type="l", lty=1, col=4, ...)
	panel.loess(x,y, span=0.5, lty=2, col=1)}
# fine tune 
ttl <- list(label="Catch trends for plaice in IV", cex=1)
yttl <- list(label=units(ple4@catch), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# legend
akey <- list(text=list(c("observed","loess(span=0.5)"), cex=0.7), border=T, lines=list(lty=c(1,2), col=c(4,1)))
# plot
print(xyplot(data~year, data=ple4@catch, panel=pfun, main=ttl, ylab=yttl, xlab=xttl, scales=ax, key=akey))


###################################################
### chunk number 10: 
###################################################
#line 178 "fleda.Rnw"
# define the panel function to include a loess fit
print(xyplot(data~year, data=catch(ple4), main=ttl, ylab=yttl, xlab=xttl, scales=ax, auto.key=TRUE, type=c("g","l","smooth")))


###################################################
### chunk number 11: 
###################################################
#line 203 "fleda.Rnw"
# compute catch proportions at age
ple4sex.pay <- pay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.pay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=4))


###################################################
### chunk number 12: 
###################################################
#line 225 "fleda.Rnw"
# compute relative catch proportion at age
ple4sex.rpay <- rpay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Relative catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.rpay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5))


###################################################
### chunk number 13: 
###################################################
#line 247 "fleda.Rnw"
# compute relative to maximum catch proportion at age
ple4sex.nay <- nay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Relative to maximum catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.nay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5))


###################################################
### chunk number 14: 
###################################################
#line 269 "fleda.Rnw"
# compute standardized catch proportion at age
ple4sex.spay <- spay(ple4sex@catch.n)
# fine tune 
ttl <- list(label="Standardized catch proportion at age for Plaice in IV", cex=1)
yttl <- list(label="age", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
# plot
print(bubbles(age~year|unit, ple4sex.spay,  main=ttl, ylab=yttl, xlab=xttl, scales=ax, bub.scale=5))


###################################################
### chunk number 15: 
###################################################
#line 294 "fleda.Rnw"
mv0(index(ple4.index))


###################################################
### chunk number 16: 
###################################################
#line 306 "fleda.Rnw"
data(ple4.indices)
lapply(ple4.indices, function(x) mv0(index(x)))


###################################################
### chunk number 17: 
###################################################
#line 323 "fleda.Rnw"
arr <- cor(index(ple4.index))
round(arr,2)


###################################################
### chunk number 18: 
###################################################
#line 336 "fleda.Rnw"
idx1 <- trim(index(ple4.indices[[1]]), age=1:8, year=1996:2008)
idx2 <- trim(index(ple4.indices[[2]]), age=1:8, year=1996:2008)
arr <- cor(idx1, idx2)
round(arr,2)


###################################################
### chunk number 19: 
###################################################
#line 356 "fleda.Rnw"

ttl <- list("Pairwise plot of age by year for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
print(splom(~data, data=index(ple4.index), panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3))


###################################################
### chunk number 20: 
###################################################
#line 384 "fleda.Rnw"
ttl <- list("Pairwise plot of age by cohort for plaice in IV (Unk.index)", cex=1)
xttl <- list("age", cex=0.8)
yttl <- list("age", cex=0.8)
# panel function
pfun <- function(x,y,...){
          panel.splom(x,y, ...)
          panel.lmline(x,y, lty=1)
        }
# plot
flc <- FLCohort(index(ple4.index))
print(splom(~data, data=flc, panel=pfun, pscales=0, main=ttl, xlab=xttl, ylab=yttl, pch=19, cex=0.3))


###################################################
### chunk number 21: 
###################################################
#line 410 "fleda.Rnw"

# let's extract the index slot from each FLIndex object in the FLIndices object.
lst <- lapply(ple4.indices, index)
# now a nice FLQuants
ple4.inds <- mcf(lst)
# scale
ple4.indsN01 <- lapply(ple4.inds, function(x){
                  arr <- apply(x@.Data, c(1,3,4,5,6), scale)
                  arr <- aperm(arr, c(2,1,3,4,5,6))
                  # small trick to fix an apply "feature"
                  dimnames(arr) <- dimnames(x)
                  x <- FLQuant(arr)
                })
ple4.indsN01 <- FLQuants(ple4.indsN01)
# stupid hack to correct names (fixed in version 2)
names(ple4.indsN01) <- names(lst)
# fine tune
ttl <- list("Surveys CPUE for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Standardized CPUE", cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
akey <- list(points=F, lines=T, columns=3, cex=0.8)
# plot
print(xyplot(data~year|factor(age), groups=qname, data=ple4.indsN01, type=c("g","l"), main=ttl, xlab=xttl, ylab=yttl, auto.key=akey, striptext=stripttl, scales=ax, as.table=TRUE, layout=c(5,2,1)))



###################################################
### chunk number 22: 
###################################################
#line 455 "fleda.Rnw"
ttl <- list(label="Catch weight at age for Place in IV", cex=1)
yttl <- list(label=units(catch.wt(ple4sex)), cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(xyplot(data~year|unit, data=catch.wt(ple4sex), groups=age, type="l", lty=1:15, col=1, main=ttl, ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 23: 
###################################################
#line 483 "fleda.Rnw"

ttl <- list(label="Maturity ogive by sex for plaice in IV", cex=1)
yttl <- list(label="%", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.7)
ax <- list(x=list(tick.number=7, cex=0.7), y=list(cex=0.7))
akey <- simpleKey(text=c("female", "male"), points=F, lines=T)
print(xyplot(data~age|as.factor(year), data=ple4sex@mat, type=c("g","l"), groups=unit, key=akey, main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl))


###################################################
### chunk number 24: 
###################################################
#line 510 "fleda.Rnw"
# compute mature and immature biomass
ple4.bmass <- bmass(ple4)
# tune plot
ttl <- list(label="Trends in biomass for mature and immature plaice in IV", cex=1)
yttl <- list(label="relative biomass", cex=0.8)
xttl <- list(cex=0.8)
ax <- list(cex=0.8)
# plot
print(xyplot(data~year, groups=qname, data=ple4.bmass, type=c("g","l"), main=ttl, auto.key=list(lines=TRUE, points=FALSE), ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 25: 
###################################################
#line 540 "fleda.Rnw"
ple4sex.cc <- logcc(ple4sex@catch.n)
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(ccplot(data~age|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1))


###################################################
### chunk number 26: 
###################################################
#line 560 "fleda.Rnw"
ple4sex.cc <- logcc(ple4sex@catch.n)
# fine tune
ttl <- list(label="Log catch curves by sex for plaice in IV", cex=1)
yttl <- list(label="log ratio", cex=0.8)
xttl <- list(cex=0.8)
stripttl <- list(cex=0.8)
ax <- list(cex=0.7)
print(ccplot(data~year|unit, data=ple4sex.cc, type="l", main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, col=1))


###################################################
### chunk number 27: 
###################################################
#line 584 "fleda.Rnw"
# compute Z
ple4z <- z(ple4@catch.n, agerng=3:6)
summary(ple4z)
t.test(ple4z)


###################################################
### chunk number 28: 
###################################################
#line 600 "fleda.Rnw"
ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
print(xyplot(data~year, data=ple4z@zy, type=c("g","l"), main=ttl, ylab=yttl, xlab=xttl))


###################################################
### chunk number 29: 
###################################################
#line 618 "fleda.Rnw"
ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
ax <- list(x=list(at=c(3:6)))
print(xyplot(data~age, data=ple4z@za, type=c("g","l"), main=ttl, ylab=yttl, xlab=xttl, scales=ax))


###################################################
### chunk number 30: 
###################################################
#line 637 "fleda.Rnw"
ple4z <- z(ple4@catch.n, agerng=3:6)
ttl <- list("Total mortality (Z) for Plaice in IV", cex=1)
xttl <- list(cex=0.8)
yttl <- list("Mean Z", cex=0.8)
print(xyplot(data~cohort, data=ple4z@zc, type=c("g","l"), main=ttl, ylab=yttl, xlab=xttl))


