########################################################################################################################
# Bluefin Projections for Cites                                                                                        #
# 23rd October 2009                                                                                                    #
# L.T. Kell                                                                                                            #
#                                                                                                                      #
# Functions                                                                                                            #
########################################################################################################################

setGeneric("setFwd", function(object, ...)
	standardGeneric("setFwd"))


## Projects for management plans
setMethod('setFwd', signature(object='FLStock'),
  setFwd<-function(object,maxage=NULL,vB=NULL,timing=c(0.5,0.5),projyrs=20,biolYrs=dim(object)$maxyear-(0:2),selYrs=dim(object)$maxyear-(1:3)){

  ## Trim to be within range
  object<-trim(object, year=range(object,"minyear"):range(object,"maxyear"),
                       age =range(object,"min")    :range(object,"max"))


  pgAges <-ac(range(object,"max"):maxage)
  prjYrs <-ac(range(object,"maxyear")+projyrs)
  histYrs<-ac(range(object,"minyear"):range(object,"maxyear"))

  ## plusgroup
  if (is.null(maxage)){
    if (!is.null(range(object,"plusgroup")))
       object<-setPlusGroup(object,range(object,"plusgroup"))} else
    object<-setPlusGroup(object,maxage,keepPlusGroup=FALSE)

  ## expand for projection years
  object<-qapply(object,function(x) expand(x,year=dims(x)$minyear:(dims(x)$maxyear+projyrs)))
  range(object,"maxyear")<-range(object,"maxyear")+projyrs

  ## fill in projection slots for biol params
  mnSlots=c("m","mat","harvest.spwn","m.spwn")
  for (i in mnSlots)
     slot(object,i)[,prjYrs]<-apply(slot(object,i)[,ac(biolYrs)],2:6,mean)
     
  ## fill in projection slots for wt params
  #for (i in c("stock.wt","catch.wt","landings.wt","discards.wt"))
  #   slot(object,i)[,prjYrs]<-apply(slot(object,i)[,ac(mnSel)],2:6,mean
  stock.wt(   object)[pgAges,histYrs][] <-vonBMass(as.numeric(pgAges)+timing[1],vB)
  catch.wt(   object)[pgAges,histYrs][] <-vonBMass(as.numeric(pgAges)+timing[2],vB)
  discards.wt(object)[pgAges,histYrs]   <-catch.wt(object)[pgAges,histYrs]
  landings.wt(object)[pgAges,histYrs]   <-catch.wt(object)[pgAges,histYrs]
  
  stock.wt(object)[     ,prjYrs ][]<-vonBMass(as.numeric(dimnames(m(object)$ages))+timing[1],vB)
  catch.wt(object)[     ,prjYrs ][]<-vonBMass(as.numeric(dimnames(m(object)$ages))+timing[2],vB)
  discards.wt(object)[  ,prjYrs ]  <-catch.wt(object)[ ,prjYrs ]
  landings.wt(object)[  ,prjYrs ]  <-catch.wt(object)[ ,prjYrs ]
  
  ## fill in selectivity slots
  lSel<-exp(apply(landingsSel(object)[,yrs],2:6, function(x) mean(log(x),na.rm=T)))
  dSel<-exp(apply(DiscardsSel(object)[,yrs],2:6, function(x) mean(log(x),na.rm=T)))
  harvest(object)[,prjYrs]   <-rep(LSel+DSel,each=length(prjYrs))
  harvest(object)[,prjYrs]   <-sweep(harvest(object)[,prjYrs],2:6,fapex(harvest(object)[,prjYrs]),"/")
  landings.n(object)[,prjYrs]<-lSel/(lSel+dSel)
  discards.n(object)[,prjYrs]<-dSel/(lSel+dSel)

  ## update
  ctrl  <-fwdControl(data.frame(quantity="f",val=fapex(object)[,-1,drop=T],year=as.numeric(dimnames(m(object))$year[-1])))
  object<-fwd(object,ctrl=ctrl, sr=list(model="mean",params=FLPar(1)),sr.residuals=rec(object))

  wt(bft[["Mean Age"]][10])   <-vonBMass(pgAge(bft[["Mean Age"]],nAges=100)+0.5,FLPar(vB))

  return(object)})