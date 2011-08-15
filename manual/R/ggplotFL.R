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
  
#### Data ##############################################################################
## example data set based on FLR
data(ple4)
  
data(ple4sex)
ple4mf=as.data.frame(stock.wt(ple4sex),drop=T)
dat   =data.frame(x=1:10,y=rnorm(10))
  
## example data set based on Med-SWO
load(paste(dirMy,"data/cas.RData",sep="/"))
  
## example data set based on bluefin
if (!file.exists(paste(dirMy,"data/bio.RData",sep="/"))){
    ## Read in data
    bio       =read.table(paste(dirMy,"inputs/BIO_f-1.STA",sep="/"),skip=1,header=T)
  
    ## simple short names
    names(bio)=c("scen","yr","lower","median","upper","mean","det","sd")
    ## scenario key, i.e. quotas and constant Fs
    scen=c(0,2000,4000,6000,8000,10000,12000,13500,14000,16000,18000,20000,22000,
             "F0.1","FMSY","FMax")
    scen=factor(scen,levels=scen)
    bio$scen=scen[bio$scen]
    
    save(bio,file=paste(dirMy,"data/bio.RData",sep="/"))} else
load(paste(dirMy,"data/bio.RData",sep="/"))
  
## GLM stuff 
load(paste(dirMy,"data/glmPos.RData",sep="/"))
  

## example data set based on bluefin
if (!file.exists(paste(dirMy,"data/rsdls.RData",sep="/"))){
  qq   =qqnorm(rstandard(glmPos),plot.it=FALSE)
  rsdls=data.frame(resStd    =rstandard(glmPos),          ## standardised
                   res       =glmPos$residuals,           ## raw
                   hatLn     =glmPos$linear.predictors,   ## linear predictor 
                   hat       =glmPos$fitted.values,       ## fitted
                   y         =glmPos$y,                   ## observed dependent variable
                   qqHat     =qq$x,                       ## expected quantile                 
                   qqObs     =qq$y)                       ## observed qunatile
    save(rsdls,file=paste(dirMy,"data/rsdls.RData",sep="/"))} else
load(paste(dirMy,"data/rsdls.RData",sep="/"))

## BFT-E Assessments
if (!file.exists(paste(dirMy,"data/ts.RData",sep="/"))){
  scenVPA=apply(expand.grid(Catch=c("Inflated","Reported"),
                            Run =c(13,15)),2,ac)
  fileVPA=apply(scenVPA,1,function(x) paste(dirMy,"inputs/bft-e/VPA",paste(x,collapse="/"),"bfte2010.c1",sep="/"))
  pgWt   =function(x) stock.wt(x)[range(x)["plusgroup"]]
  ts =FLStocks(mlply(fileVPA,readVPA2Box,.progress="text"))
  ts =ldply(ts, function(x) as.data.frame(x[[c("ssb","fbar","rec","catch","stock","mnSwt","pgWt")]]), .progress="text")[,c("qname","year","data","iter")]

  save(ts,file=paste(dirMy,"data/ts.RData",sep="/"))} else
load(paste(dirMy,"data/ts.RData",sep="/"))
     
## BFT-E projections 
if (!file.exists(paste(dirMy,"data/ts.RData",sep="/"))){
  scenPrj=apply(expand.grid(Implementation=c("Error","Perfect"),
                            Catch         =c("Inflated","Reported"),
                            Run           =c(13,15)[1],
                            Recruitment   =c("High","Low","Medium")[3],
                            Steepness     =c("0.5","0.75","0.9")[2]),2,ac)
  filePrj=apply(scenPrj,1,function(x) paste(dirMy,"inputs/bft-e/proj",paste(x,collapse="/"),sep="/"))
     
  rel=function(x){
       Ref=readPro2Box(x,type="ref")
       Out=readPro2Box(x,type="out")
      
       msy.=subset(Ref,refpt="msy")
       rel.=transform(Out,ssb    =ssb/msy.[iter+1,"ssb"],
                          rec    =rec/msy.[iter+1,"rec"],
                          harvest=fapex/msy.[iter+1,"harvest"],
                          yield  =yield/msy.[iter+1,"yield"])[,c("scen","iter","year","ssb","harvest")]
       
       return(rel.)}
  
   prj=mdply(filePrj,rel,.progress="text")
   prj=cbind(scenPrj[prj$X1,],prj)
   save(prj,file=paste(dirMy,"data/ts.RData",sep="/"))} else
load(paste(dirMy,"data/ts.RData",sep="/"))  
#### End Data ###########################################################################
  
#### Demos ##############################################################################
p  =  ggplot(ple4mf)
p1 = p + geom_point(aes(age,data))
p2 = p + geom_boxplot(aes(factor(age),data))
p3 = p + geom_point(aes(age,data,group=unit,colour=unit),position="jitter")
p4 = p3 + stat_smooth(aes(age,data,group=unit,colour=unit))
  
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
cas1=qplot(len, data = cas,geom="histogram",weight=n)
cas =transform(cas, decade=(year %% 10),yr=year-(year %% 10))
cas2=ggplot(ddply(cas,c("year"),transform, n=n/sum(n))) + 
        geom_histogram(aes(len,weight=n))+
        facet_grid(decade~yr)
cas3=ggplot(ddply(cas,c("year"),transform, n=n/sum(n))) + 
        geom_histogram(aes(len,weight=n,fil=gear))+
        facet_grid(decade~yr)
  
###### Time series
ts1=qplot(yr,mean,data=subset(bio, scen %in% seq(0,22000,2000)))
ts2=qplot(yr,mean,data=subset(bio, scen %in% seq(0,22000,2000)))
ts3=ts2+geom_line(aes(yr,mean,group=scen,col=scen))
ts4=ts3+geom_line(aes(yr,mean,group=scen,col=scen))+
              scale_x_continuous(limits=c(2000,2022))
bio2=melt(subset(bio,scen %in% seq(0,22000,2000), select=c(scen,yr,lower:upper)), 
          measure.vars=c("lower","median","upper"))

ts5=ggplot(bio2) + 
         geom_line(aes(yr,value,group=scen:as.factor(variable),col=scen)) +
         scale_x_continuous(limits=c(2000,2022))

## glms
#plot(glmPos)
glm =ggplot(data.frame(glmPos$data,rsdl=residuals(glmPos),hat=fitted(glmPos)))
glm1=glm+geom_point(aes(hat,rsdl))
glm2=glm1+stat_smooth(aes(hat,rsdl))
  
glm2=glm1+geom_point(aes(hat,rsdl))+stat_smooth(aes(hat,rsdl))+facet_wrap(~qtr)
glm2=glm1+geom_point(aes(hat,rsdl))+stat_smooth(aes(hat,rsdl))+facet_wrap(~area)
glm2=glm1+geom_point(aes(hat,rsdl))+stat_smooth(aes(hat,rsdl))+facet_grid(qtr~area,scale="free")
  
## QQ-plot
dia1=ggplot(rsdls) + geom_point(aes(qqHat,qqObs),size=0.5)   +
                      opts(title = "Normal Q-Q Plot")         + 
                      scale_x_continuous(name="Theoretical Quantiles") +
                      scale_y_continuous(name="Sample Quantiles")  +
                      geom_abline(intercept=0, slope=1)
  
dia2=ggplot(rsdls) + geom_point(aes(hat,resStd),size=0.5) + 
                      stat_smooth(aes(hat,resStd),method="gam") +
                      opts(title="Error Distributions")    + 
                      scale_x_continuous(name="Predicted") +
                      scale_y_continuous(name="Standardised Residuals")
  
dia3=ggplot(rsdls) + geom_point(aes(hatLn,res), size=0.5) + 
                      stat_smooth(aes(hatLn,res),method="gam") +
                      opts(title="Assumed Variance") + 
                      scale_x_continuous(name="Predicted on Link") +
                      scale_y_continuous(name="Absolute Residuals")
dia4=ggplot(rsdls) + geom_point(aes(hatLn,y), size=0.5) + 
                      stat_smooth(aes(hatLn,y),method="gam") +
                      opts(title="Link Function") + 
                      scale_x_continuous(name="Predicted on Link") +
                      scale_y_continuous(name="Observed")
  
# vplayout =function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
#   
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(2,2)))
# print(dia1, vp=vplayout(1,1))
# print(dia2, vp=vplayout(2,1))
# print(dia3, vp=vplayout(1,2))
# print(dia4, vp=vplayout(2,2))
  
##### Mapping
world      = data.frame(map("world", plot=FALSE)[c("x","y")])
world1     = ggplot(world) + geom_path(aes(x,y))  
world2     = world1 + coord_map() 
world3     =world2 + geom_path(aes(x,y))  + 
                   scale_x_continuous(limits=c(-20,20)) + 
                   scale_y_continuous(limits=c(-20,20)) + coord_map() 
  
                      
##### FLR
## FLstock
plot(ple4)
plot(ple4sex)
   
## FLStocks          
plot(FLStocks(Male=ple4sex[,,"male"],Female=ple4sex[,,"female"]))
            
# library(ggplotFL)
# 
# ## FLstock
# plot(ple4)
# plot(ple4sex)
#  
# ## FLStocks          
# plot(FLStocks(Male=ple4sex[,,"male"],Female=ple4sex[,,"female"]))
       
## Diagnostics
            
## Kobe
            
## benefits
            
## extra plots
            
            