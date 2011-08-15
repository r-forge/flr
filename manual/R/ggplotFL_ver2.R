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

###### Time series
SSBSmry=ddply(tsSSB,.(Catch,Run,year),function(x) quantile(x$data, prob=c(0.25,0.5,0.75)))
names(SSBSmry)[4:6]=c("P25","Median","P75")

    
# Define the top and bottom of the errorbars 
limits <- aes(ymax = P75, ymin=P25) 

p  = ggplot(SSBSmry, aes(colour=Run, y=Median, x=year)) 
p1 = p  + geom_point() + geom_errorbar(limits, width=0.2) 
p2 = p1 + geom_pointrange(limits) 
p3 = p2 + geom_crossbar(limits, width=0.2) 
  
# If we want to draw lines, we need to manually set the 
# Runs which define the lines - here the Runs in the 
# original dataframe 
p4 = p3 + geom_line(aes(Run=Run)) + geom_errorbar(limits, width=0.2) 

p5 = qplot(year, data, data=(subset(ts,scen==1 & qname=="ssb"))) 
p6 = p5 + stat_summary(fun.data = "mean_cl_boot", colour = "red") 

#New variables produced by the statistic

    # geom_errorbar: error bars
    # geom_pointrange: range indicated by straight line, with point in the middle
    # geom_linerange: range indicated by straight line
    # geom_crossbar: hollow bar with middle indicated by horizontal line
    # stat_smooth: for continuous analog

#Examples

# Basic operation on a small dataset 
p7 <- ggplot(aes(year, data, group=Catch, colour=Catch), data=SSB)+geom_point(size=0.2)

#p + ylim(15, 30) 
#Warning: Removed 9 rows containing missing values (stat_summary).
  
# Instead use coord_cartesian 
#p + coord_cartesian(ylim = c(15, 30)) 
  
  
# You can supply individual functions to summarise the value at 
# each x: 
  
stat_sum_single <- function(fun, geom="point", ...) 
   stat_summary(fun.y=fun, geom=geom, colour="black", size = 1, ...) 
    
p8 = p7 + stat_sum_single(mean) + facet_grid(Catch~Run)  
p9 = p8 + stat_sum_single(mean, geom="line") 
p10= p9 + stat_sum_single(median) 
p11= p10 + stat_sum_single(sd) 
p12= p11 + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour = "red") 
p13 = p7 + aes(colour = factor(Catch:Run),group=factor(Catch:Run)) + stat_summary(fun.y = mean, geom="line") 
  
  
# Alternatively, you can supply a function that operates on a data.frame. 
# A set of useful summary functions is provided from the Hmisc package: 
 
stat_sum_df <- function(fun, geom="crossbar", ...) 
   stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...) 
 
p14 = p13 + stat_sum_df("mean_cl_boot") 
p15 = p13 + stat_sum_df("mean_sdl") 
p16 = p13 + stat_sum_df("mean_sdl", mult=1) 
p17 = p13 + stat_sum_df("median_hilow") 
  
# There are lots of different geoms you can use to display the summaries 
p18 = p13 + stat_sum_df("mean_cl_normal") 
p19 = p13 + stat_sum_df("mean_cl_normal", geom = "errorbar") 
p20 = p13 + stat_sum_df("mean_cl_normal", geom = "pointrange") 
p21 = p13 + stat_sum_df("mean_cl_normal", geom = "smooth") 
  

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