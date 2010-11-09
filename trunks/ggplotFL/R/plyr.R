alignSR<-function(x,recAge){
    if (recAge>0)
      sr<-data.frame(Recruits=x[-(1:recAge),               "rec"],
                     SSB     =x[-(dim(x)[2] +1-(1:recAge)),"ssb"])
    else
      sr<-x[,c("Recruits","SSB")]

    return(sr)}

stdz  <-function(x) ((x-mean(x))/sd(x))

minMax<-function(x) (x-min(x))/diff(range(x))

#t1    <-ddply(ctE,c("month","year"),transform,y=stdz(cpue))
#t2    <-ddply(ctE,c("month","year"),transform,y=minMax(cpue))
