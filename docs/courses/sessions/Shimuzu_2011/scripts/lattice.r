# An example of plotting with lattice for FLR

library(FLCore)

data(ple4)
data(ple4sex)
data(ple4.indices)

# trellis plots work on data frames
# but you can pass an FLQuant directly
head(as.data.frame(stock.wt(ple4)))

# A straight forward xyplot
xyplot(data~year|age, data=stock.wt(ple4), type="l")

# convert the grouping factor to type factor to display it properly in the strip
xyplot(data~year|as.factor(age), data=stock.wt(ple4), type="l")

# putting a legend inside plot area  ## new ##
xyplot(data~year, groups=age, data=stock.wt(ple4), type="l", 
       key=list(x=0.25, y=0.95, lines=list(lty=1, col=1:10), 
                text=list(lab=as.character(1:10)), columns=4))

xyplot(data~year|as.factor(age)+as.factor(unit), data=stock.wt(ple4sex), type="l")

# Using the groups argument to put multiple plots into each panel
xyplot(data~year|as.factor(age), groups=unit, data=stock.wt(ple4sex),type="l")

# Box and whisker plots use bwplot
bwplot(data~year, data=stock.wt(ple4))

# You can also use boxplot - however for this method you will have to 
# manually convert the FLQuant to a data.frame 
boxplot(data~age, data=as.data.frame(stock.wt(ple4)))

# Add some more lines - from a Von-B fit
vb <- nls(data~((Linf*(1-exp(-k*(age-t0))))^3/1000), data=as.data.frame(stock.wt(ple4)),start=list(Linf=50, k=0.2, t0=0))

lines(predict(vb, list(age=1:15)), col="red", lwd=2)
title(main="North Sea Plaice Stock Weights at Age")

#more than one conditioning factor
xyplot(data~year|age+qname, groups=unit, data=FLQuants(stk.wt=stock.wt(ple4sex),cat.wt=catch.wt(ple4sex)),type="l")


# An example of a panel function
pfun <- function(x,y,...){
  panel.xyplot(x,y, type='p', ...)
  panel.loess(x,y,...)
}
xyplot(data~year|age, 
       data=stock.wt(ple4)[2:5,],panel=pfun)

# coerce the conditioning factor to charater to get the ages printed in the panel
xyplot(data~year|as.character(age), 
       data=stock.wt(ple4)[2:5,],panel=pfun)




# This time for the two sex object
pfun <- function(x,y,...) {
  panel.bwplot(x,y,...)
  panel.lines(predict(nls(y~((Linf*(1-exp(-k*(x))))^3)/1000,data=data.frame(x=as.numeric(x),y=as.numeric(y)),start=list(Linf=50,k=0.2)),list(x=1:15)))
}

bwplot(data~age|unit,data=as.data.frame(FLQuants(stock.wt(ple4sex))),panel=pfun, horizontal=FALSE)


# Adding a legend
xyplot(data~year|age+qname, groups=unit, data=FLQuants(stk.wt=stock.wt(ple4sex),cat.wt=catch.wt(ple4sex)),type="l", col=c('blue','red'), key=list(space='top', lines=list(lty=1, col=c('blue','red')), text=list(legend=c('males','females'))))


# Putting multiple plots on one image
df <- cbind(as.data.frame(stock.wt(ple4)), residuals=summary(vb)$residuals)
pfun1 <- function(x,y,...){
  panel.xyplot(x,y,col='grey',...)
  panel.loess(x,y,col='black',...)
  panel.abline(h=0, lty=2, col='lightgrey',...)
}
plot1 <- xyplot(residuals~age, data=df, type=c('g','p'), panel=pfun1, main='residuals')

pfun2 <- function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.lines(predict(nls(y~((Linf*(1-exp(-k*(x))))^3)/1000,data=data.frame(x=as.numeric(x),y=as.numeric(y)),start=list(Linf=50,k=0.2)),list(x=1:15)), col='black')
}
plot2 <- xyplot(data~age, data=df, type=c('g','p'), panel=pfun2, col='grey', main='fitted model')

print(plot2, position=c(0,0,1,0.5),more=T)
print(plot1, position=c(0,0.5,1,1))




