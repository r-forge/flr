# Functions and routines to check dimensions of objects - particularly Fleets and Biols

chkDms<-function(biol,flt,yrs){
#browser()
    biolDms<-unlist(dims(biol))
    fltDms <-unlist(dims(flt))
    #        names(fltDms)[8:11]<-names(biolDms)[8:11]
    ##dims(bt4[,'sol'])

    jntDms<-c("min","max","minyear","maxyear","unit","season","area","iter")

    if (!all(fltDms[jntDms]==biolDms[jntDms]))
    return("dims in fleets and biol don't match")
    if (!all(yrs %in% fltDms["minyear"]:fltDms["maxyear"]))
    return("Target years and years in fleets and biol don't match")

    yrs<-(min(yrs)-as.integer(fltDms["min"])):max(yrs)
    if (!all(yrs %in% fltDms["minyear"]:fltDms["maxyear"]))
    return("Earlier years needed in biol for SSB calculation")

    res<-as.numeric(fltDms[jntDms])
    names(res)<-names(fltDms[jntDms])

    return(res)}

chkFleets<-function(fleets){
   nmfleet =names( fleets)
   nfleet  =length(fleets)

   nmmetier=dims(fleets)$metiers
   nmetier =length(nmmetier)

   nmspp   =dims(fleets)$catches
   nspp    =length(nmspp)
   
   vl <-c("min","max","minyear","maxyear","unit", "season", "area", "iter")         
   vls<-c("min","max","minyear","maxyear","units","seasons","areas","iters")         
   ref<-unlist(dims(fleets))[vls]
   for (iFlt in nfleet){
      if (!all(unlist(dims(fleets[[1]]))[vls]==ref)) print("stuff") else print("no stuff") 
      for (iMet in nmetier){
         if (!all(unlist(dims(fleets[[1]][[1]]))[vl]==ref)) print("stuff") else print("no stuff")  
         for (iSpp in nspp){
            if (!all(unlist(dims(fleets[[1]][[1]][[1]]))[vl]==ref)) print("stuff") else print("no stuff") 
            }}}
   }    
    
chkTrgtArrayIters <- function(object,trgtArray,sr)
{
    if (is(object,'FLlst')) object <- object[[1]]
    its<-sort(unique(c(length(dimnames(trgtArray)$iters), dims(object)$iter, length(dimnames(sr$params[[1]])$iter), length(dimnames(sr$residuals[[1]])$iter))))
    if (length(its)>2 | (length(its)>1 & its[1]!=1)) stop("Iters not 1 or n") 
    if (length(its)==2 & dimnames(trgtArray)$iter == 1){
        dmns<-dimnames(trgtArray)
        dmns$iters<-1:its[2]
        trgtArray<-array(trgtArray,dim=unlist(lapply(dmns,length)),dimnames=dmns)}
    return(trgtArray)
}

