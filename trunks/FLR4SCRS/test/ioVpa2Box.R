#readPro2Box.STA<-function(x)
dirVPA<-"\\\\Iccatsec\\meetings\\YEAR2010\\BFT_ASSESS\\Analyses\\Methods\\VPA\\East\\Runs\\Results\\Bootstraps\\Reported\\run13\\1"
 

naa<-readBinary(paste(dirVPA,"naa.out",sep="/"),list(age=1:40,year=1950:2009,iter=1:501))
faa<-readBinary(paste(dirVPA,"faa.out",sep="/"),list(age=1:40,year=1950:2009,iter=1:501))
maa<-readBinary(paste(dirVPA,"maa.out",sep="/"),list(age=1:40,year=1950:2009,iter=1:501))
caa<-readBinary(paste(dirVPA,"caa.out",sep="/"),list(age=1:40,year=1950:2009,iter=1:501))
expZ<-exp(-faa*.5-maa*.5)

#### Data
wt=c(6.01,10.6,18.73,33.16,53.1,74.81,96.56,119.9,145.67,178.8813007,206.4997581,
      234.0221203,261.1411374,287.6099665,313.2361634,337.8751075,361.4233907,383.8125019,
      405.0029975,424.9792559,443.7448496,461.3185264,477.7307684,493.0208812,507.2345606,
      520.4218796,532.6356447,543.9300665,554.3597021,563.978624,572.8397811,580.9945172,
      588.4922216,595.3800848,601.7029417,607.5031835,612.8207246,617.693012,622.1550679,
      626.2395568,629.9768707,633.3952269,636.5207741,639.3777038,641.9883629,644.3733668,
      646.5517114,648.5408811,650.3569536,652.0147007,653.5276835,654.908343,656.1680855,
      657.3173624,658.3657457,659.3219972,660.1941345,660.9894907,661.7147709,662.3761039,
      662.9790905,663.5288469,664.0300463,664.486956,664.9034719,665.2831502,665.6292366,
      665.944693,666.2322217,666.4942881,666.7331412,666.9508324,667.1492325,667.3300477,
      667.4948338)[1:40]/1000
waa<-as.FLQuant(wt,dimnames=list(age=1:40,year=1950:2009))

mat<-expand(as.FLQuant(c(0,0,0,0.5,1,1,1,1,1,1),dimnames=list(age=1:10,year=1950:2009)),age=1:40)
mat[11:40]<-1
      ggplot(as.data.frame(apply(apply(naa*waa*mat*expZ,c(2,6),sum),2,iter,1)))+geom_line(aes(year,data))+geom_line(data=as.data.frame(ssbVPA),aes(year,data),col="red")


dat<-read.table(paste(x,iFile,sep=""),skip=1,header=F)
 
######## East
prjE<-NULL
for (i in prjEOptions$Catch)
  for (j in prjEOptions$Recruits)
    for (k in prjEOptions$Run){
       dirNow<-paste(dirEPrj,i,j,k,sep="/")

       proBrp<-data.frame(Catch=i,Recruits=j,Run=k,read.table(paste(dirNow,"BENCH-1.OUT",sep="/"),header=F,skip=1))
       proYld<-data.frame(Catch=i,Recruits=j,Run=k,read.table(paste(dirNow,"YIELD-1.OUT",sep="/"),header=F))
       proFav<-data.frame(Catch=i,Recruits=j,Run=k,read.table(paste(dirNow,"Fapex-1.OUT",sep="/"),header=F))
       proSsb<-data.frame(Catch=i,Recruits=j,Run=k,read.table(paste(dirNow,"SSBIO-1.OUT",sep="/"),header=F))

       names(proBrp)<-c("Catch","Recruits","Run",nmsRef)

       res<-data.frame(Catch   =rep(proYld[,1],21),
                       Recruits=rep(proYld[,2],21),
                       Run     =rep(proYld[,3],21),
                       Scen    =rep(proYld[,4],21),
                       Iter    =rep(proYld[,5],21),
                       Year    =rep(2005:2025, each= dim(proYld)[1]),
                       F       =unlist(proFav[,61:81]),
                       SSB     =unlist(proSsb[,61:81]),
                       F0.1    =rep(proBrp[,"f0.1"]),
                       B0.1    =rep(proBrp[,"ssb0.1"]))

      res<-res[res$Scen>1,]
      res<-merge(res,eScen)[,c("TAC","Catch","Recruits","Run","Year","F","SSB","F0.1","B0.1")]

      prjE<-rbind(prjE,res)}

######## East
chkE<-NULL
for (i in prjEOptions$Catch)
  for (j in prjEOptions$Recruits)
    for (k in prjEOptions$Run){
       dirNow<-paste(dirEPrj,i,j,k,sep="/")

       proSsb<-data.frame(Catch=i,Recruits=j,Run=k,read.table(paste(dirNow,"SSBF01-1.STA",sep="/"),skip=1,header=F))

       names(proSsb)[4:11]<-c("Scen","Year","lowerCL","Median","upperCL","mean","det","sd")

       chkE<-rbind(chkE,proSsb)}


      chkE<-chkE[chkE$Scen>1,]
      chkE<-merge(chkE,eScen)

chkE<-merge(chkE,eScen)[,c("TAC",names(chkE)[-1])]
