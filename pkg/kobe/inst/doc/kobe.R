### R code from vignette source 'kobe.Rnw'

###################################################
### code chunk number 1: kobe.Rnw:39-46
###################################################
library(plyr)
library(ggplot2)
library(FLCore)
library(FLAdvice)

data(kobe.ts)
head(kobe.ts)


###################################################
### code chunk number 2: kobe.Rnw:51-53
###################################################
smry.ts=ddply(kobe.ts, .(TAC,year), function(x) data.frame(ssb=median(x$ssb),harvest=median(x$harvest)))
head(smry.ts)


###################################################
### code chunk number 3: kobe.Rnw:57-60
###################################################
kb=ggplot(subset(kobe.ts)) + geom_boxplot(aes(factor(year),ssb)) +
                             facet_wrap(~TAC)
kb


###################################################
### code chunk number 4: kobe.Rnw:63-64
###################################################
print(kb)


###################################################
### code chunk number 5: kobe.Rnw:69-70
###################################################
ggplot(smry.ts)+geom_line(aes(year,ssb,    colour=factor(TAC),group=TAC))


###################################################
### code chunk number 6: kobe.Rnw:73-74
###################################################
ggplot(smry.ts)+geom_line(aes(year,harvest,colour=factor(TAC),group=TAC))


###################################################
### code chunk number 7: kobe.Rnw:77-78
###################################################
ggplot(smry.ts)+geom_line(aes(year,ssb,    colour=factor(TAC),group=TAC))


###################################################
### code chunk number 8: kobe.Rnw:81-82
###################################################
ggplot(smry.ts)+geom_line(aes(year,harvest,colour=factor(TAC),group=TAC))


###################################################
### code chunk number 9: kobe.Rnw:90-93
###################################################
kpp=kobe(subset(smry.ts,year<=2010)) + 
    geom_path(aes(ssb,harvest,colour=year),size=2)+
    geom_point(aes(ssb,harvest), data=subset(kobe.ts,year==2010 & TAC==0))


###################################################
### code chunk number 10: kobe.Rnw:100-102
###################################################
kobe(subset(smry.ts,year>=2010)) + 
  geom_path(aes(ssb,harvest,colour=factor(TAC),group=TAC),size=2)


###################################################
### code chunk number 11: kobe.Rnw:107-111
###################################################
kobe.ts=data.frame(kobe.ts, kobeP(kobe.ts$ssb,kobe.ts$harvest))

head(kobe.ts)
kobeM(subset(kobe.ts,year>=2010)[,c("year","TAC","p")])


###################################################
### code chunk number 12: kobe.Rnw:116-117
###################################################
kobeM(subset(kobe.ts,year>=2010)[,c("year","TAC","b")])


###################################################
### code chunk number 13: kobe.Rnw:120-121
###################################################
kobeM(subset(kobe.ts,year>=2010)[,c("year","TAC","f")])


###################################################
### code chunk number 14: kobe.Rnw:126-135
###################################################
library(xtable)
res=kobeM(subset(kobe.ts,year>=2010)[,c("year","TAC","f")])

head(res)
head(kobeShade(res))

t.=kobeShade(t(res))

xtable(kobeShade(t(res)))


###################################################
### code chunk number 15: regresults
###################################################



###################################################
### code chunk number 16: kobe.Rnw:143-153
###################################################
dat=transform(subset(kobe.ts,year>2010),red=collapsed,green=p,yellow=1-p-collapsed)
dat=ddply(dat,.(TAC,year),function(x) data.frame(red=mean(x$red),green=mean(x$green),yellow=mean(x$yellow)))
dat=melt(dat,id.vars=c("year","TAC"))

pie = ggplot(subset(dat,year>=2011 & year<=2020 & TAC==4000), aes(x ="", y=value, fill = factor(variable))) + 
        geom_bar(width = 1) + 
        coord_polar(theta = "y") +
        labs(fill='Kobe Quadrant') + xlab('') + ylab('')       +
        scale_fill_manual(values=c("red","green","yellow"))    + 
        facet_grid(TAC~year) 


###################################################
### code chunk number 17: kobe.Rnw:156-157
###################################################
pie


