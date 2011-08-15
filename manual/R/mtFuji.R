## Introduction to ggplot2 #####################################################
## ggplot2 is an R package for graphics

# Whenever you first want to plot during an R session you will need to enter the
# command:
# library(ggplot2)

# If you do not have ggplot installed, you can install the package in the R
# session by entering the command:
# install.packages("ggplot2")

# A reference manual can be found on Hadley Wickham's website
# http://had.co.nz/ggplot2/

# Alterantively look on google

# An example of a scatter plot, using a simple dataset #########################
dat<-data.frame(x=1:10,y=rnorm(10))

## In R the default graphics function is plot
plot(x,y,data=dat)

## In ggplot there are options qplot or ggplot

#### qplot #####################################################################
## i.e. quick plot that implements ggplot with a plot like interface
## very similar to default R graphics syntax
# For additional help and examples use the command
?qplot
qlot(x,y,data=dat)


#### ggplot ####################################################################
## A more general function that allows great flexibility but is initially a
#  bit more complex
# For additional help and examples use the command
?ggplot
ggplot(data)

## To plot you must use add layers e.g.geoms of various type, e.g.
## for points or lines

?geom_point
?geom_line

#	along with the "geom." the "aesthetic" has to be provided,
# in the above case for x & y
gplot(data) + geom_point(aes(x,y))


## Catch-at-size ###############################################################
#	Mediterranean swordfish catch-at-size
load("cas.RData")

#	What does it contain
head(cas)

# Histograms can explore how one (or more) categorical variables are distributed.
qplot(len, data=cas, geom="histogram")

qplot(len,data=cas,geom.="histogram",weight=n)

qplot(len, data = cas,geom="histogram",weight=n, binwidth = 10

## You can combine qplot & ggplot, i.e.
## create the basic plot then make it more beuatiful
p<-qplot(len, data = cas,geom="histogram",weight=n, binwidth = 10

p+facet_wrap(~year)

################################################################################

## Saving Plots ################################################################
# There are a few ways to save your plots in R.
# If the plot has already been rendered, then use
?dev.copy
?ggsave

# A benefit of using ggplot is the flexibility it allows of building up a plot
# in layers

## functions to calculate some wavey lines
f <- function(x) ifelse (abs(x)<=1, x^4-x^2+6, 12/(abs(x)+1))
g <- function(x) ifelse (abs(x)<=2, 0.5*cos(2*x*pi)+7/2, 12/(abs(x)+1))

# 1st layer, sets plot range
p<-ggplot(data.frame(x=c(-8,8)), aes(x))
p

# geom_tile produces a range of colours in the plot,
# in this case varying over the y dim
p<-p+geom_tile(aes(x, y, fill=y*3),
                   data=data.frame(expand.grid(x=c(-4,4),
                                               y=seq(0.05, 6.95, 0.1))))
p

# superimpose a function, i.e. for the x supplied in data.frame(x=c(-8,8))
# calculate Y from the functions f & g
p<-p+stat_function(fun = f, geom='area', fill='white', colour=NA, n=1000) +
     stat_function(fun = g, geom='area', fill='black', colour=NA, n=1000)
p

# Tile again but make it a bit prettier
p<-p+ geom_tile(aes(x, y, fill=z),
            transform(data.frame(expand.grid(x=seq(-7.95, 7.95, 0.1),
                                             y=seq(0.05, 6.95, 0.1))),
                                             z=-sqrt((5-x)^2+(5.5-y)^2)),
            alpha=0.25)
p

# Add a point
p<-p+ geom_point(aes(x, y), data.frame(x=5, y=5.5), size=30, colour="yellow")
p

# add some more objects using stat_function
p<-p+stat_function(fun = function(x)sin(x*3)/2+2, geom="area",
                                                  fill="darkgreen",
                                                  colour=NA)
p

# use YlOrRd colormap from RColorBrewer, this customises the colour
library(RColorBrewer)
p<-p+scale_fill_gradientn(colour=rev(brewer.pal(7,"YlOrRd")))
p

# Modify default labels and legend by getting rid of them
p<-p+opts(legend.position = "none",
       axis.text.x = theme_blank(),
       axis.text.y = theme_blank(),
       axis.title.x = theme_blank(),
       axis.title.y = theme_blank())
p

# Based on
# "Draw Mt. Fuji in R" posted on "SIGUNIANG'S BLOG".
# see http://siguniang.wordpress.com/2010/12/19/rdraw-mt-fuji-in-r/
