library(FLSURBA)

data(codiva)
data(codiva.RV)
data(codiva.control)

codiva<-convert6d(codiva)
codiva.RV[[1]]<-convert6d(codiva.RV[[1]])
codiva.RV[[2]]<-convert6d(codiva.RV[[2]])
codiva.RV[[3]]<-convert6d(codiva.RV[[3]])

codiva.surba<-FLSURBA(codiva,codiva.RV,control=codiva.control)
codiva      <-codiva+codiva.surba

#diagnostic plots
plot(      codiva.surba)  #there is a bug in this, calls the wrong plot, source("FLAssess-methods.R) then it works!
plotFitted(codiva.surba)
plotRes(   codiva.surba)
plotResN(  codiva.surba,index=1)
plotResYr( codiva.surba,index=1)
plotIndex( codiva.surba,index=1)
qqResYr(   codiva.surba,index=1)

summary( codiva.surba)
show(    codiva.surba)

## validation ##################################################################
validate.surba<-function(i,stock,indices,options,fbar,ref)
  {
  j<-ref["ref.age"]==options[i,"ref.age"] & ref["smooth"]==options[i,"smooth"] & ref["q1"]==options[i,"q1"]

  control       <-new("FLSURBA.control")
  control@fbar  <-fbar
  control@smooth<-options[i,"smooth"]
  control@refage<-options[i,"ref.age"]
  control@z.spwn<-stock@harvest.spwn

  indices[[1]]@index.q["1",]<-options[i,"q1"]
  indices[[2]]@index.q["1",]<-options[i,"q1"]
  indices[[3]]@index.q["1",]<-options[i,"q1"]

  surba<-FLSURBA(stock,indices,control)
return(surba)

  cbind(round(ssb(surba+stock),2),surba@stock.n[1,],surba@r.var,surba@z.var,surba@z.bar,ref[j,"ssb","rec","rec.var","ssb","z.bar","z.var"])

  cbind(surba@z[,1]/surba@z[,options[i,"ref.age"]],ref[j,"sel.1","sel.2","sel.3","sel.4","sel.5","sel.6"][1,])
  }

#doesn't work yet
codiva.retro<-retro(codiva,codiva.RV,control=codiva.FLSURBA.control,retro=1)

plot(codiva.retro)

load("D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\cod4\\cod4.RData")
load("D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\cod4\\cod4.indices.RData")
load("D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\cod4\\cod4.surba.ref.RData")
load("D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\cod4\\cod4.options.RData")

for (i in 1:length(cod4.options[1,])
  validate.surba(i, cod4, cod4.indices,  cod4.options, c(min=2,max=4), cod4.surba.ref)



## Irish Sea Plaice ############################################################
ple7a    <-trim(read.FLStock("D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\ple7a\\index.dat"),ages=1:8,years=1987:2004)
ple7a.RV<-read.FLIndices(file ="D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\ple7a\\fleets.dat",
                         file2="D:\\FLR\\Packages\\Tests\\Validation\\FLSURBA\\ple7a\\ssbidx.dat",
                         type="ICA")

ple7a<-trim(ple7a,years=1987:2004)
ple7a.RV<-trim(ple7a.RV,years=1987:2004)

for (i in 1:length(ple7a.RV)) {
   ple7a.RV[[i]]@index.q[]  <-1.0
   ple7a.RV[[i]]@index.var[]<-1.0
   }
   
ple7a.RV[[3]]@range[c("startf","endf")]<-c(0.750,0.850)
ple7a.RV[[7]]@range[4:5]<-c(1996,2003)

ple7a.surba<-FLSURBA(ple7a,ple7a.RV,FLSURBA.control(refage=4,smooth=1.0))
ple7a.surba<-FLSURBA(ple7a,FLIndices(ple7a.RV[[3]]),FLSURBA.control(refage=4,smooth=1.0))
ple7a      <-ple7a+ple7a.surba


