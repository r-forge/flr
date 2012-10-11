library(kobe)

data(sims)
data(assmt)

### Plots ###################################################################################################
## Historic Assessment Plots
assmt1=ggplot(assmt)                               + 
  geom_hline(aes(yintercept=1),col="red",size=2)  + 
  geom_line( aes(year,stock,group=iter,col=iter)) +
  opts(legend.position="none")

assmt2=ggplot(assmt)                                + 
  geom_hline(aes(yintercept=1),col="red",size=2)   + 
  geom_line(aes(year,harvest,group=iter,col=iter)) +
  scale_y_continuous(limits=c(0,4))+
  opts(legend.position="none")

assmt3=ggplot(assmt)                              +
  geom_hline(aes(yintercept=1),col="red")         +
  geom_boxplot(aes(factor(year),harvest))         +
  scale_x_discrete(breaks=seq(1980,2010,10))      +
  xlab("Year") + ylab(expression(SSB/B[MSY]))       

### comparison ########################################################################################
## time series
### tracks
tmp=subset(sims, year %in% 1980:2020, select=c(Run,TAC,iter,year,stock,harvest))
trks=kobeTrks(tmp,c("Run","TAC","year"))

head(trks)

trks1=ggplot(subset(trks,TAC==15000 & year<=2010))+
  geom_line(aes(year,stock,linetype=Percentile),col="blue")+
  geom_line(aes(year,harvest,linetype=Percentile),col="red")+
  scale_linetype_manual(values=c(2,1,2)) +
  facet_wrap(~Run,ncol=2)

## Kobe phase plots ####################################################################################
pts=subset(sims,year==2010 & TAC==15000)
kobe1=kobePhase(pts)+geom_point(aes(stock,harvest,col=Run))

kobe2=kobe1+
  facet_wrap(~Run) + 
  opts(legend.position = "none")

kobe3=kobe2+geom_point(aes(stock,harvest,col=Run),data=subset(trks,year==2010 & TAC==15000 & Percentile=="50%"),col="black",size=3)+
      scale_y_continuous(limits=c(0,4))
      
kobe3+geom_path( aes(stock,harvest),data=subset(trks,year>=1960 & year<=2010 & Percentile=="50%" & TAC==15000))

#fig3 + geom_point(aes(x,y,alpha=freq/max(freq),col=Run),data=cbind(pts,calcFreq(pts$stock,pts$harvest,x.n=5)),size=1)
#fig3 + stat_contour(aes(x = x, y = y, z = z), data=calcDensity(pts$stock,pts$harvest,n=10), breaks=c(1,2,5),col="cyan")

kobe3 + geom_path(aes(x,y,group=level),colour="blue",
                    data=ddply(pts,.(Run), function(pts) calcProb(pts$stock,pts$harvest,prob=c(0.7,.5,.25)))) 


# stock density plot
ggplot(pts) + 
  geom_density(aes(x = stock,  y =  ..count.., group=Run, fill=Run, alpha=0.4))
ggplot(pts) + 
  geom_density(aes(x = harvest, y =  ..count.., group=Run, fill=Run, alpha=0.4))

ggplot(pts) + 
  geom_density(aes(x = stock,  y =  -..count.., group=Run, fill=Run, alpha=0.4)) +
  geom_density(aes(x = stock,  y =  ..count.., group=Run, fill=Run), fill="grey", col="grey", position = "stack") 
ggplot(pts) + 
  geom_density(aes(x = harvest,  y =  -..count..,  group=Run, fill=Run, alpha=0.4)) +
  geom_density(aes(x = harvest,  y =  ..count.., group=Run, fill=Run), fill="grey", col="grey", position = "stack") 

kobePhaseMar(transform(pts,group=Run))           

### Pies ############################################################################################
pie.dat=with(subset(sims,year==2010 & TAC==15000),cbind(Run,kobeP(stock,harvest)))[,c("Run","red","green","yellow")]
pie.dat=ddply(melt(pie.dat,id.vars="Run"),.(Run,variable), function(x) data.frame(value=mean(x$value)))


## pie charts
ggplot(pie.dat, aes(x ="", y=value, fill = factor(variable))) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y") +
  labs(fill='Kobe Quadrant') + xlab('') + ylab('')       +
  scale_fill_manual(values=c("red","green","yellow"))    + 
  facet_wrap(~Run)                                   + 
  scale_x_discrete(breaks=NULL)                          +
  scale_y_continuous(breaks=NULL) 

### K2SM #####################################################################################################
library(directlabels)
  
### K2SM
sim2x=function(x){
  x=cbind(x[,c("year","TAC")],kobeP(x$stock,x$harvest)[,c("green","overFished","overFishing")])
  x=transform(x,overFished=1-overFished,overFishing=1-overFishing)
  x=melt(x,id.vars=c("year","TAC"))
  x=ddply(x, .(variable), function(x) k2smFn2(x[,-3]))
  x}

sim3x=function(x){
  x=cbind(x[,c("year","TAC")],kobeP(x$stock,x$harvest)[,c("green","overFished","overFishing")])
  x=transform(x,overFished=1-overFished,overFishing=1-overFishing)
  x=melt(x,id.vars=c("year","TAC"))
  x=ddply(x,c("year","TAC","variable"), function(obj) data.frame(value=mean(obj$value)))
  x}

image  =list(levels=seq(0.0,1.0,0.05),
             col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8)))

x=sim2x(subset(sims,year%in%2010:2020))
v = ggplot(aes(x=x,y=y,z=w),data=x)                        +
           geom_tile(aes(x,y,fill=z))                         +
           scale_fill_manual(values=image$col,guide="none")   +
           stat_contour(aes(colour = ..level..),size=1.2,  breaks=c(0.6,0.7,0.8,0.9))     +
           scale_colour_gradient(low="grey", high="black", breaks=c(0.6,0.7,0.8,0.9),labels=c(0.6,0.7,0.8,0.9),limits=c(0.6,1))    +
           facet_wrap(~variable,ncol=1)                       +
           xlab("Year")+ylab("TAC") 
  print(direct.label(v))

tabs=with(subset(sims,year %in% 2010:2020), cbind(year,TAC,kobeP(stock,harvest)))
cast(tabs,TAC~year,value="green",      mean)
cast(tabs,TAC~year,value="overFished", function(x) mean(1-x))
cast(tabs,TAC~year,value="overFishing",function(x) mean(1-x))


latex(shade(k2smTab[[1]],pct=""),rowlabel="TAC",
                                 rowname =dimnames(k2smTab[[1]])$TAC,
                                 caption ="Kobe II Strategy Matrix, $P(F\\leq F_{MSY})$.")

latex(shade(k2smTab[[2]],pct=""),rowlabel="TAC",rowname=dimnames(k2smTab[[1]])$TAC,
      caption="Kobe II Strategy Matrix, $P(SSB\\geq B_{MSY})$).")
latex(shade(k2smTab[[3]],pct=""),rowlabel="TAC",rowname=dimnames(k2smTab[[1]])$TAC,caption="Kobe II Strategy Matrix, $P(F\\leq F_{MSY})$ and $P(SSB\\geq B_{MSY})$.")
