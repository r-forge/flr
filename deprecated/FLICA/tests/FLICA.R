### does the buisness ##############################################################################
validate.ica<-function(key,stock,indices,control,option,ica.ref){
      ## key      run
      ## stock    FLStock object
      ## indices  FLIndices object
      ## control  FLICA.control object
      ## option   data frame with run options
      ## ica.ref  reference data sets from exe for comparison

      ## replace catch.n with landings, explicitly assuming no discarding
      #stock<-no.discards(stock)

      ## truncates age range
      if ("truncate"  %in% names(option))
        if (as.logical(option[key,"truncate"])){
          stock               <-trim(stock,  age=0:7)
          indices[[2]]        <-trim(indices[[2]],age=1:7)
          control@lambda.age  <-control@lambda.age[1:8]
          }

      ## change ICA options
      control@sep.nyr   <-as.integer(option[key,"sep.nyr"])
      control@sep.sel   <-option[key,"sep.sel"]

      ## change all weights to 1.0
      if ("truncate"  %in% names(option))
        if (as.logical(option[key,"wt"])){
          control@lambda.age[] <-as.double(1.0)
          for (i in 1:length(indices)) indices[[i]]@index.var[]<-1.0
          }

     if ("sep.2"          %in% names(option)) control@sep.2           <-as.integer(option[key,"sep.2"])
     if ("sep.gradual"    %in% names(option)) control@sep.gradual     <-as.logical(option[key,"sep.gradual"])
     if ("catch age 1 wt" %in% names(option)) control@lambda.age[1]   <-option[key,"catch age 1 wt"]
     if ("ssb wt"         %in% names(option)) indices[[1]]@index.var[]<-1.0/option[key,"ssb wt"]
     if ("UKBTS wt"       %in% names(option)) indices[[1]]@index.var[]<-1.0/option[key,"UKBTS wt"]
     if ("DARDS wt"       %in% names(option)) indices[[2]]@index.var[]<-1.0/option[key,"DARDS wt"]
     if ("DARDA wt"       %in% names(option)) indices[[3]]@index.var[]<-1.0/option[key,"DARDA wt"]

      ## run ICA
      ica        <- FLICA(stock, indices, control)

      ## put results into data frame
      flica.ref<-cbind(as.data.frame(ica@stock.n)[,-(3:5)],as.data.frame(ica@harvest)[,-(1:5)])
      names(flica.ref)<-c("age","year","fl.n","fl.f")

      ## merge with reference data set
      comp <- merge(flica.ref,ica.ref[ica.ref[,"key"]==key,])[,-5]

      ## compare FLICA to reference data set
      return(list(!any(abs(comp[,"f"]/comp[,"fl.f"]-1.0)>0.025),
                  !any(abs(comp[,"n"]/comp[,"fl.n"]-1.0)>0.025)))
      }
### End does the buisness ##########################################################################

### Test data ##################################################################
#FLStock objects
data(NEAMac.test)
data(her4.test)
data(ple7a.test)

#FLFIndices objects
data(NEAMac.indices.ica.test)
data(her4.indices.ica.test)
data(ple7a.indices.ica.test)

#Reference N's & F's
data(NEAMac.ica.ref)
data(her4.ica.ref)
data(ple7a.ica.ref)

#ica control options
data(NEAMac.ica.control)
data(her4.ica.control)
data(ple7a.ica.control)
### End test data ##############################################################

### Run all tests ##############################################################
for (i in 1:16)
  if (!any(validate.ica(i, NEAMac.test,  NEAMac.indices.ica.test,  NEAMac.ica.control,  NEAMac.ica.ref))) print("error")

for (i in 1:16)
  if (!any(validate.ica(i, her4.test,  her4.indices.ica.test,  her4.ica.control,  her4.ica.ref))) print("error")

for (i in 1:16)
  if (!any(validate.ica(i, ple7a.test, ple7a.indices.ica.test, ple7a.ica.control, ple7a.ica.ref))) print("error")
### End run all tests ##########################################################

#run ica
her4.ica   <-FLICA(her4.test, her4.test.indices)
ple7a.ica  <-FLICA(ple7a.test,ple7a.test.indices)
NEAMac.ica <-FLICA(NEAMac.test, NEAMac.test.indices)

#diagnostic plots
plot(      her4.ica)
plotFitted(her4.ica,index=1)
plotRes(   her4.ica)
plotResN(  her4.ica,index=1)
plotResYr( her4.ica,index=1)
plotIndex( her4.ica,index=1)
qqResYr(   her4.ica,index=1)

plot(      ple7a.ica)
plotFitted(ple7a.ica,index=1)
plotRes(   ple7a.ica)
plotResN(  ple7a.ica,index=1)
plotResYr( ple7a.ica,index=1)
plotIndex( ple7a.ica,index=1)
qqResYr(   ple7a.ica,index=1)

plot(      NEAMac.ica)
plotFitted(NEAMac.ica,index=1)
plotRes(   NEAMac.ica)
plotResN(  NEAMac.ica,index=1)
plotResYr( NEAMac.ica,index=1)
plotIndex( NEAMac.ica,index=1)
qqResYr(   NEAMac.ica,index=1)

#Retrospectives
her4.retro<-retro(her4.test, her4.test.indices,retro=2)
ple7a.retro<-retro(ple7a.test,ple7a.test.indices,retro=2)
NEAMac.retro<-retro(NEAMac.test, NEAMac.test.indices,retro=2)
