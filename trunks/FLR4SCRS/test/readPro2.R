readPro2<-function(dir){
       proBrp<-data.frame(read.table(paste(dir,"BENCH-1.OUT",sep="/"),header=F,skip=1))
       proYld<-data.frame(read.table(paste(dir,"YIELD-1.OUT",sep="/"),header=F))
       proFav<-data.frame(read.table(paste(dir,"Fapex-1.OUT",sep="/"),header=F))
       proSsb<-data.frame(read.table(paste(dir,"SSBIO-1.OUT",sep="/"),header=F))

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
      res}
