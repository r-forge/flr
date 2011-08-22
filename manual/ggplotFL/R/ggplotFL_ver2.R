#### Libraries #########################################################################
library(ggplot2)
library(FLCore)
library(FLBRP)
library(FLln2z)
library(mapproj)
library(DBI)
library(RSQLite)
library(gam)
  
#dirMy="/home/lkell/flr/manual/ggplotFL"

source(paste(dirMy,"R/dataIn.R",sep="/"))
 
 
#### Demos ##############################################################################
p  =  ggplot(ple4mf)
p1 = p + geom_point(aes(age,data))
p2 = p + geom_boxplot(aes(factor(age),data))
p3 = p + geom_point(aes(age,data,Run=unit,colour=unit),position="jitter")
p4 = p3 + stat_smooth(aes(age,data,Run=unit,colour=unit))
  
p5= p3 + scale_x_continuous(name="Age")      + 
         scale_y_log10(name="Weight (Kg)") +
         scale_colour_manual("Sex",values=c("blue","red"))  
p5$data =transform(p5$data, decade=year-(year %% 10))
p6= p5 + facet_wrap(~decade) 
  
  
## fig 1
plot( dat$x,dat$y)
## fig 2
qplot(dat$x,dat$y)
    
##### CAS #####################################
## add new variables for plotting
cas    =transform(cas, decade=(year %% 10),yr=year-(year %% 10))
cas    =ddply(cas,.(year),transform, prop=n/sum(n))

cas1   =ggplot(cas) + geom_histogram(aes(len,weight=n))
cas2   =ggplot(cas) + geom_histogram(aes(len,weight=prop),binwidth=5) + facet_grid(decade~yr)
cas3   =ggplot(cas) + geom_histogram(aes(len,weight=prop,Run=gear,colour=gear),binwidth=5) + facet_grid(decade~yr)

###### Time series ######################################################################
SSBSmry=ddply(SSB,.(Catch,Run,year),function(x) quantile(x$data, prob=c(0.25,0.5,0.75)))
names(SSBSmry)[4:6]=c("P25","Median","P75")

tsp  = ggplot(SSBSmry, aes(colour=Run:Catch, y=Median, x=year)) 

# Define the top and bottom of the errorbars 
limits=aes(ymax=P75, ymin=P25) 

tsp1 = tsp + geom_point() + geom_errorbar(limits, width=0.2) 
tsp2 = tsp + geom_pointrange(limits) 
tsp3 = tsp + geom_crossbar(limits, width=0.3) 
  
# If we want to draw lines, we need to manually set the 
# group which define the lines - here the Runs in the 
# original dataframe 
tsp4 = tsp + geom_line(aes(group=Run:Catch)) + geom_crossbar(limits, width=0.3)

#New variables produced by the statistic

    # geom_errorbar: error bars
    # geom_pointrange: range indicated by straight line, with point in the middle
    # geom_linerange: range indicated by straight line
    # geom_crossbar: hollow bar with middle indicated by horizontal line
    # stat_smooth: for continuous analog

#Examples

# Basic operation on a small dataset 
tsp5 = qplot(year, data, data=(subset(ts,scen==1 & qname=="ssb"))) 
tsp6 = tsp5 + stat_summary(fun.data = "mean_cl_boot", colour = "red") 
tsp7 = ggplot(aes(year, data, group=Catch, colour=Catch), data=SSB)+geom_point(size=0.2)

#p + ylim(15, 30) 
#Warning: Removed 9 rows containing missing values (stat_summary).
  
# Instead use coord_cartesian 
#p + coord_cartesian(ylim = c(15, 30)) 
  
  
# You can supply individual functions to summarise the value at 
# each x: 
  
stat_sum_single <- function(fun, geom="point", ...) 
   stat_summary(fun.y=fun, geom=geom, colour="black", size = 1, ...) 
    
tsp8  = tsp7 + stat_sum_single(mean) + facet_grid(Catch~Run)  
tsp9  = tsp8 + stat_sum_single(mean, geom="line") 
tsp10 = tsp9 + stat_sum_single(median) 
tsp11 = tsp10 + stat_sum_single(sd) 
tsp12 = tsp11 + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour = "red") 
tsp13 = tsp7 + aes(colour = factor(Catch:Run),group=factor(Catch:Run)) + stat_summary(fun.y = mean, geom="line") 

tsp14 = tsp7 + stat_sum_single(quantile, prob=seq(0.5), geom="line") + facet_grid(Catch~Run) 
  
# Alternatively, you can supply a function that operates on a data.frame. 
# A set of useful summary functions is provided from the Hmisc package: 
 
stat_sum_df <- function(fun, geom="crossbar", ...) 
   stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...) 
 
tsp15 = tsp13 + stat_sum_df("mean_cl_boot") 
tsp16 = tsp13 + stat_sum_df("mean_sdl") 
tsp17 = tsp13 + stat_sum_df("mean_sdl", mult=1) 
tsp18 = tsp13 + stat_sum_df("median_hilow") 
  
# There are lots of different geoms you can use to display the summaries 
tsp19 = tsp13 + stat_sum_df("mean_cl_normal") 
tsp20 = tsp13 + stat_sum_df("mean_cl_normal", geom = "errorbar") 
tsp21 = tsp13 + stat_sum_df("mean_cl_normal", geom = "pointrange") 
tsp22 = tsp13 + stat_sum_df("mean_cl_normal", geom = "smooth") 
  

#© Hadley Wickham 2011





##### Mapping
world      = data.frame(map("world", plot=FALSE)[c("x","y")])
world1     = ggplot(world) + geom_path(aes(x,y))  
world2     = world1 + coord_map() 
world3     =world2 + geom_path(aes(x,y))  + 
                   scale_x_continuous(limits=c(-20,20)) + 
                   scale_y_continuous(limits=c(-20,20)) + coord_map() 
  
                      
##### FLR
library(ggplotFL)

## FLstock
flr1 =plot(ple4)
flr2 =plot(ple4sex)
flr3 =plot(ple4sex)+facet_grid(qname~unit,scale="free")
flr4 =plot(FLStocks(Male=ple4sex[,,"male"],Female=ple4sex[,,"female"]))
            
## Diagnostics
            
## Kobe
            
## TEP

## Saving Plots ################################################################
# There are a few ways to save your plots in R.
# If the plot has already been rendered, then use
?dev.copy
?ggsave

# Not run: 
ggsave(file="length-hist.pdf")
ggsave(file="length-hist.png")
ggsave(ratings, file="ratings.pdf")
ggsave(ratings, file="ratings.pdf", width=4, height=4)

# make twice as big as on screen
ggsave(ratings, file="ratings.pdf", scale=2)


