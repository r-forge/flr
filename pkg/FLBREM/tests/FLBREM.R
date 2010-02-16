
## set up adult index based upon data in biomass.dat
adult          <-new("FLIndex")
adult@index    <-FLQuant(c(11321.158,6453.406, 2757.817, 6628.615,
                           3382.021, 4883.180, 2928.896, 3580.033,
                           2213.013, 3804.548, 4446.052, 4283.051,
                           2662.460, 2445.235, 2837.943, 2179.990,
                           1543.419, 1406.579, 1945.253, 1540.172,
                           1600.833, 1176.406, 1517.689,  864.215, 
                           757.389,   574.303,  350.916,  642.183, 
                           508.770,   416.287), 
                         dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))

adult@index.var<-(adult@index*0.1)^2
adult@effort     <-FLQuant(NA, dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))
adult@catch.wt   <-FLQuant(0.1,dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))
adult@catch.n    <-FLQuant(NA, dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))
adult@sel.pattern<-FLQuant(NA, dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))
adult@index.q    <-FLQuant(1.0,dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))
adult@range      <-c(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=30,startf=NA,endf=NA)
adult@distribution<-"lognormal"

## set up recruit index based upon data in biomass.dat
rec            <-new("FLIndex")
rec@index	   <-FLQuant(c( 78.190, 87.007,162.363, 79.164,
                          46.060, 57.238, 85.494, 93.601,
                         127.899, 95.948, 25.021, 19.744,
                          30.046, 50.679, 72.730, 163.492,
                          50.497, 71.341, 28.652, 16.616,
                           8.463, 27.282, 22.713, 65.268,
                          66.887, 44.852, 42.591, 14.647,
                          19.435), 
                        dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@index.var   <-(rec@index*0.1)^2
rec@effort      <-FLQuant(NA, dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@catch.wt    <-FLQuant(0.1,dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@catch.n     <-FLQuant(NA ,dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@sel.pattern <-FLQuant(NA ,dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@index.q     <-FLQuant(1 ,dimnames=list(age="all",year=2:30,unit="unique",area="all",season="unique"))
rec@range       <-c(min=NA,max=NA,plusgroup=NA,minyear=2,maxyear=30,startf=NA,endf=NA)
rec@distribution<-"lognormal"

adult.wt<-FLQuant(c(2.478971188,2.566384348,2.326029797,2.512758328,
                    2.302318093,2.282235852,2.337671418,2.182951119,
                    2.025319754,2.074163469,2.021647907,2.418825726,
                    2.514857698,2.697132948,2.684717447,2.400913575,
                    1.580623414,1.582520350,1.695715471,1.889253913,
                    2.021324057,2.303285488,2.433082137,2.279184155,
                    1.806501402,1.299182357,1.223166107,1.349378375,
                    1.570416008,1.764217102),
                  dimnames=list(age="rec",year=1:30,unit="unique",area="all",season="unique"))
 
rec.wt  <-FLQuant(0.459,dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))

## run FLBREM
t.<-FLBREM(rec,adult,control=FLBREM.control(r.index.q=1,r.index.cv=0.1,adult.q=1,adult.cv=0.2))

