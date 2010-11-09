library(ggplot2)
library(FLR4SCRS)
library(FLash)
library(FLBRP)
library(DBI)
library(RSQLite)

######## East #############################################################################################################################

#### Dirs & Spec etc. #####################################################################################################################
#dirPrj <-"\\\\Iccatsec/meetings/YEAR2010/BFT_ASSESS/Analyses/Methods/VPA/East/Projections/Results"
#dirVPA <-"\\\\Iccatsec/meetings/YEAR2010/BFT_ASSESS/Analyses/Methods/VPA/East/Projections/Results/Bootstraps"

dirData<-"c:\\Stuff\\ICCAT\\SCRS\\2010\\BFT\\analysis\\VPA\\East\\Projections\\RData"
dirPrj <-"c:\\Stuff\\ICCAT\\SCRS\\2010\\BFT\\analysis\\VPA\\East\\Projections\\Results"
dirVPA <-"c:\\Stuff\\ICCAT\\SCRS\\2010\\BFT\\analysis\\VPA\\East\\Runs\\Results\\Bootstraps\\Reported"

prjOptions<-list(Catch   =c("Inflated","Reported"),
                 Recruits=c("LowRec","MedRec","HighRec"),
                 Run     =c("Run13.perf","Run13.Sel20","Run15.perf","Run15.Sel20","Run17.perf","Run17.Sel20","Run18.perf","Run18.Sel20"))

Scen       <-c(0,2000,4000,6000,8000,10000,12000,13500,14000,16000,18000,20000)
names(Scen)<-1:12

recs       <-c("Low","Medium","High")
names(recs)<-c("LowRec","MedRec","HighRec")

vpa       <-c("13","13","15","15","17","17","18","18")
names(vpa)<-c("Run13.perf","Run13.Sel20","Run15.perf","Run15.Sel20","Run17.perf","Run17.Sel20","Run18.perf","Run18.Sel20")

impl       <-c("Perfect","20%","Perfect","20%","Perfect","20%","Perfect","20%")
names(impl)<-c("Run13.perf","Run13.Sel20","Run15.perf","Run15.Sel20","Run17.perf","Run17.Sel20","Run18.perf","Run18.Sel20")
###########################################################################################################################################


#### Read Time series by iteration, year and scenario #####################################################################################
#### Deterministic runs
ePrjDet<-NULL
for (i in prjOptions$Catch)
   for (j in prjOptions$Recruits)
      for (k in prjOptions$Run){
         tst<-try(res <-rbind(data.frame(Catch=i,Recruits=recs[j],VPA=vpa[k],Implementation=impl[k],
                                          readPro2Sta(paste(dirPrj,i,j,k,sep="/")))), silent=T)
         
         if(class(tst) =='try-error') cat(i,j,k,"\n",sep=",") else ePrjDet<-rbind(ePrjDet,res)}
                  
ePrjDet$Catch         <-as.factor(ePrjDet$Catch         )
ePrjDet$Recruits      <-as.factor(ePrjDet$Recruits      )
ePrjDet$VPA           <-as.factor(ePrjDet$VPA           )
ePrjDet$Implementation<-as.factor(ePrjDet$Implementation)
ePrjDet$TAC           <-Scen[ePrjDet$Scen]
ePrjDet               <-ePrjDet[,c("Catch","Recruits","VPA","Implementation","TAC","Year","Median","det")]

head(ePrjDet)     
ePrjDetPlot<-ggplot(ePrjDet[ePrjDet$TAC %in% c(seq(0,20000,4000),13500),])  + geom_line(aes(Year,det,col=Recruits,group=Recruits:Implementation:Catch, lty=Implementation, size=Catch)) + 
                   facet_grid(VPA~TAC) + 
                   scale_x_continuous(name="Year", limits=c(1960,2030), breaks=seq(1960,2030,20),  labels=ac(seq(1960,2030,20))) +
                   scale_y_continuous(name="SSB",  limits=c(0,400000),  breaks=c(0,200000,400000), labels=c(0,200000,400000)) +
                   scale_size_manual(values=c(1,0.5), name="Catch")

save(ePrjDet,ePrjDetPlot,file=paste(dirData,"Eprj.RData",sep="/"))

#### Monte Carlo runs
## DB
SQLite(max.con = 16, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE)

## connect DB
dbMC  <-paste(dirPrj,"Eprj.dbf",sep="")
conMC <-dbConnect(dbDriver("SQLite"), dbname=dbMC)

apnd=FALSE
for (j in prjOptions$Recruits)
  for (k in prjOptions$Run)
    for (i in prjOptions$Catch) {
         cat("Reading", i,j,k,"\n",sep=",")

            tst1<-try(rpt<-readVPA2Brp(paste(dirPrj,i,j,k, "BENCH-1.OUT", sep="/"))["f0.1",c(1,4)])
            tst2<-try(ts <-readPro2(   paste(dirPrj,i,j,k, sep="/"),1950))

            if(class(tst1) =='try-error' | class(tst2) =='try-error')
               cat("Error!",i,j,k,"\n",sep=",")
            else {
               rpt   =data.frame(iter=as.data.frame(rpt[,1,])[,3],f0.1=as.data.frame(rpt[,1,])[,4],b0.1=as.data.frame(rpt[,2,])[,4])
               ts$TAC=Scen[ts$scen]
               ts    =data.frame(Catch=i,Recruits=recs[j],VPA=vpa[k],Implementation=impl[k],merge(ts,rpt)[,-2])
               
               ### All iters
               dbWriteTable(conMC, "prj", ts, append=apnd)

               ### Summaries
               B<-with(ts,aggregate(ssb/b0.1,  by=list(Catch=Catch,Recruits=Recruits,VPA=VPA,Implementation=Implementation,TAC=TAC,Year=year),quantile, probs=c(0.90,0.5,0.10)))
               F<-with(ts,aggregate(fapex/f0.1,by=list(Catch=Catch,Recruits=Recruits,VPA=VPA,Implementation=Implementation,TAC=TAC,Year=year),quantile, probs=c(0.90,0.5,0.10)))

               prjB <-rbind(data.frame(B[,1:6],SSB=B[,7][,1],Quantile="90%"),
                            data.frame(B[,1:6],SSB=B[,7][,2],Quantile="50%"),
                            data.frame(B[,1:6],SSB=B[,7][,3],Quantile="10%"))

               prjF <-rbind(data.frame(F[,1:6],F  =F[,7][,1],Quantile="90%"),
                            data.frame(F[,1:6],F  =F[,7][,2],Quantile="50%"),
                            data.frame(F[,1:6],F  =F[,7][,3],Quantile="10%"))

               prj<-merge(prjB, prjF)

               dbWriteTable(conMC, "prjSmry", prj, append=apnd)
               }

            if (!apnd) apnd=TRUE
            }

dbListTables(conMC)
file.info(dbMC)

## close and clean up databases
#dbDisconnect(conMC)
#dbRemoveTable(conMC)


ggplot(TS[TS %in% c(seq(0,20000,4000),13500),])  + geom_line(aes(Year,det,col=Recruits,group=Recruits:Implementation:Catch, lty=Implementation, size=Catch)) +
                   facet_grid(VPA~TAC) +
                   scale_x_continuous(name="Year", limits=c(1960,2030), breaks=seq(1960,2030,20),  labels=ac(seq(1960,2030,20))) +
                   scale_y_continuous(name="SSB",  limits=c(0,400000),  breaks=c(0,200000,400000), labels=c(0,200000,400000)) +
                   scale_size_manual(values=c(1,0.5), name="Catch")


## Read Assessment data
mat   <-c(0,0,0,0.5,1,1,1,1,1,1,rep(1,30))
m     <-c(0.490,0.240,0.240,0.240,0.240,0.200,0.175,0.150,0.125,0.100,rep(0.1,30))
waa   <-read.table(paste(dirPrj,"\\Reported\\MedRec\\Run15.perf\\waa.txt",sep="/"))
swt   <-FLQuant(t(array(unlist(waa[waa[,2]==1,4:dim(waa)[2]]),dim=c(60,40))),dimnames=list(age=1:40,year=1950:2009))

run15 <-readVPA2Box(paste(dirVPA,"\\run15\\1",sep=""),"Bfte2010.csv",m=m,mat=mat,swt=swt,m.spwn=0.5)

#### Historic reference points
rfpHist<-brpByYr(run15)

refpts(rfpHist)[1:4,1:4]

brpDF<-rbind(data.frame(year=dimnames(rfpHist)$year,Quantity="F",
                        data=rfpHist["f0.1","ssb",    ]),
             data.frame(year=dimnames(rfpHist)$year,Quantity="SSB",
                        data=rfpHist["f0.1","harvest",]))
                        
ggplot(brpDF)+geom_line(aes(as.numeric(ac(year)),as.numeric(B0.1)))
ggplot(brpDF)+geom_line(aes(as.numeric(ac(year)),as.numeric(f0.1)))

csel        <-as.data.frame(catch.sel(rfpHist))[,c("age","iter","data")]
csel$year   <-as.numeric(ac(csel$iter)) + 1949
csel$decade <-as.numeric(ac(csel$year - csel$year %% 10 ))
csel$year   <-csel$year %% 10

csel.       <-as.data.frame(catchSel(iter(run15,1)))[,c("age","year","data")]
csel.$year  <-as.numeric(ac(csel.$year))
csel.$decade<-as.numeric(ac(csel.$year - csel.$year %% 10 ))
csel.$year  <-csel.$year %% 10

p<-ggplot(csel) + geom_line(            aes(age,data,group=year,colour=year)) + 
                  geom_point(data=csel.,aes(age,data,group=year,colour=year)) + 
                  facet_wrap(~decade)+scale_x_continuous(limits=c(1,11))
