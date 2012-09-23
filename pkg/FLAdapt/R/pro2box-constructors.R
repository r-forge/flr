# constructors - constructor methods for pro2box
# pro2box/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setMethod('pro2box', signature(object='missing'),
          function(object,...){
            
             res=new("pro2box")    
            
            return(res)})
  
  setMethod('pro2box', signature(object='FLStock'),
    function(object,dir=tempdir(),...){
      args = list(...)
  
      # empty object
      object[]= NA
      dims    = dims(object)
      res     = new("pro2box")
  
      stock( res)  = stock(  object)
      harvest(res) = harvest(object)
      range(res)   =unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))
  
      # Load given slots
     for(i in names(args))
  	  slot(res, i) = args[[i]]
  
      return(res)})
  
  setMethod('pro2box', signature(object='FLAdapt'),
            function(object,dir=tempdir(),...){
              args = list(...)
              
              # empty object
              object[]= NA
              dims    = dims(object)
              res     = new("pro2box")
              
              stock( res)  = stock(  object)
              harvest(res) = harvest(object)
              range(res)   =unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))
              
              # Load given slots
              for(i in names(args))
                slot(res, i) = args[[i]]
              
              return(res)})
  
  setMethod('pro2box', signature(object="character"),
    function(object,...){
      args = list(...)
  
      res=pro2box()
      
      object="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BFT/2012/VPA/projections/Run_2/Reported/2012/low/Prj12.ctl"
      
      ctrl=scan(object,what=as.character(),sep="\n")
      ctrl=str_trim(ctrl[substr(ctrl,1,1)!="#"])
  
      s2n=function(x) as.numeric(str_trim(strsplit(x," ")[[1]][1]))
      
      res@options["model"]   =s2n(ctrl[1])
      res@options["nbox"]    =s2n(ctrl[2])
      res@options["niters"]  =s2n(ctrl[3])
      res@options["c1"]      =s2n(ctrl[4])
      res@options["seed"]    =s2n(ctrl[5])
      res@options["patch"]   =s2n(ctrl[6])
      res@options["yrbox"]   =s2n(ctrl[7])
      res@options["srbox"]   =s2n(ctrl[8])
      res@options["sex"]     =s2n(ctrl[9])
      res@options["wt"]      =s2n(ctrl[10])
      
      ## get files
      fls=mdply(rev(ctrl[length(ctrl)-0:9]), function(x) strsplit(gsub("\t+", " ", str_trim(x))," +")[[1]][1:2])[,-1]
      fls=transform(fls,V2=tolower(V2))
      
      ## check
      mdply(fls$V2,getExt)[,2] == c("0"="txt","1"="out")[fls$V1]
      
      res@files=fls$V2
      
      s2n=function(x) as.numeric(str_trim(strsplit(x," ")[[1]]))
      
      res@range[c("min","max")]        =s2n(ctrl[12])[1:2]
      res@range[ "plusgroup"]          =ctrl[12][1:2]
      res@range[c("minyear","maxyear")]=ctrl[12]
      res@range[c("minselr","maxsel")] =ctrl[12]
      
   
      mat=gsub("\t+", " ", ctrl[18])
      mat=FLQuant(as.numeric(strsplit(mat," +")[[1]]),dimnames=list(age=res@range["min"]:res@range["max"]))
      
      res@mat=mat
      res@grw=getGrw(ctrl[19:20])
      
      
      res@desc="test"
      
      # Load given slots
      for(i in names(args))
        slot(res, i) = args[[i]]
  
     return(res)})
  
  getGrw=function(x){
    require(stringr)
    
    grw=lapply(strsplit(x," +"),as.numeric)
    
    par=FLPar(NA,dimnames=list(params=c("linf","k","t0","a","b","offset"),wt=c("stock","catch"),iter=1))
    
    par[c("linf","k","t0","a","b"),1]=grw[[1]][c(2:4,6:7)]
    par[c("linf","k","t0","a","b"),2]=grw[[2]][c(2:4,6:7)]
    par["offset",1]=(grw[[1]][7]-grw[[1]][8])/12
    par["offset",2]=(grw[[1]][7]-grw[[1]][8])/12
    
    return(par)}
  
  getMat=function(x){
    require(stringr)
  
    
    return(mat)}
