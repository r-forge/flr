library(FLBREM)
load("D:/FLR/Packages/FLBREM/data/  ")
source("D:/FLR/Packages/FLBREM/R/FLBREM.r")
 
 #rec_cv1<-window(rec_cv1,start=1958,end=2001)

rec_cv1@index    <-rec_cv1@index[,-1]
rec_cv1@catch.wt <-rec_cv1@catch.wt[,-1]
rec_cv1@index.var<-rec_cv1@index.var[,-1]

rec_cv1@range["minyear"]<-as.integer(dimnames(rec_cv1@index)$year[1])

cntrl<-FLBREM.control(r.mu=c(min=1,max=10000000,phase=1),   	        ## average recruitment
                      r.cv=c(min=-6,max=10,phase=1),		              ## cv recruitement logNormal scale
                      g0=c(min=-5,max=0.01,phase=2),		              ## log(growth) in year 1
                      g.sigma=c(min=-6,max=0,phase=1),		            ## sd(Z_devs) 
                      adult.q=c(min=.99,max=1.01,phase=-1),	          ## Catchability adults; set to 1
                      r.index.q=c(min=.001,max=1,phase=1),		        ## Catchability recruits 
                      adult.cv=c(min=0.19,max=0.21,phase=-1),  	      ## CV of adult survey errors on log scale
                      r.index.cv=c(min=0.02,max=2,phase=2),	 	        ## CV of recruit survey errors on log scale
                      stock0=c(min=1,max=50000000,phase=1))		        ## biomass in starting year t=1


FLBREM(rec_cv1,adult_cv1,cntrl)


##generate *.dat files for running ABMD exe
#write.table(cbind(1:45,1,as.numeric(adult_cv1@index)),file="c:\\temp\\a.txt",row.names=FALSE,col.names=FALSE)
#write.table(cbind(2:45,1,as.numeric(rec_cv1@index)),  file="c:\\temp\\r.txt",row.names=FALSE,col.names=FALSE)

#run ABMD exe
#bmod2sd -ind  test_cv1.dat -ainp test_cv1.pin

