gislasim=function(par,t0=-0.1,a=0.00001,b=3,bg=b,ato95=1,sl=2,sr=5000,a1=2){
#browser()
  names(dimnames(par)) <- tolower(names(dimnames(par)))

  ## growth parameters
  if (!("t0"    %in% dimnames(par)$params)) par=rbind(par,FLPar("t0"    =t0))
  if (!("a"     %in% dimnames(par)$params)) par=rbind(par,FLPar("a"     =a))
  if (!("b"     %in% dimnames(par)$params)) par=rbind(par,FLPar("b"     =b))
  if (!("bg"    %in% dimnames(par)$params)) par=rbind(par,FLPar("bg"    =bg))
  if (!("k"     %in% dimnames(par)$params)) par=rbind(par,FLPar("k"=3.15*par["linf"]^(-0.64))) # From Gislason et al 2008, all species combined
 
  if (!("ato95" %in% dimnames(par)$params)) par=rbind(par,FLPar("ato95" =ato95))
  if (!("sl"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sl"    =sl))
  if (!("sr"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sr"    =sr))
 
  ## maturity parameters from http://www.fishbase.org/manual/FishbaseThe_MATURITY_Table.htm
  if (!("fec" %in% dimnames(par)$params)) par=rbind(par,FLPar("t0"=1.0))
  par=rbind(par,FLPar(a50=0.72*par["linf"]^0.93))
  par=rbind(par,FLPar(c("asym"=1.0),iter=dims(par)$iter))
  
  par["a50"]=invVonB(par,c(par["a50"]))
  
  ## selectivity guestimate
  selPar=par["a50"]+a1
  
  dimnames(selPar)$params[1]="a1"
 
  par=rbind(par,selPar)
 
  attributes(par)$units=c("cm","kg","1000s")
  
  return(par)}


setUnits=function(res, par){

    units=attributes(par)$units
    #browser()
    allUnits=list("params"=      "",          
               "refpts"=         "",            
               "fbar"=           "",        
               "fbar.obs"=       "",    
               "landings.obs"=   paste(units[2],units[3]),
               "discards.obs"=   paste(units[2],units[3]),
               "rec.obs"=        units[3],         
               "ssb.obs"=        paste(units[2],units[3]),
               "stock.obs"=      paste(units[2],units[3]),
               "profit.obs"=     NA,     
               "revenue.obs"=    NA,    
               "landings.sel"=   "",    
               "discards.sel"=   "", 
               "bycatch.harvest"="",        
               "stock.wt"=       units[2],     
               "landings.wt"=    units[2],     
               "discards.wt"=    units[2],      
               "bycatch.wt"=     units[2],               
               "m"=              "",             
               "mat"=            "proportion", 
               "harvest.spwn"=   "proportion",          
               "m.spwn"=         "proportion",    
               "availability"=   "proportion",           
               "price"=          NA,           
               "vcost"=          NA,           
               "fcost"=          NA)            

    
    units(res)[names(allUnits)]=allUnits
    
    return(res)}

#### Life History Generator ####################################################
lh=function(par,
            growth       =vonB,
            fnM          =function(par,len,T=290,a=FLPar(c(a=-2.1104327,b=-1.7023068,c=1.5067827,d=0.9664798,e=763.5074169),iter=dims(par)$iter))
                                    exp(a[1]+a[2]*log(len) + a[3]*log(par["linf"]) + a[4]*log(par["k"]) + a[5]/T),
            fnMat        =logistic,
            fnSel        =dnormalFn,
            sr           =list(model="bevholt",s=0.75,v=1e3),
            range  =c(min=1,max=40,minfbar=1,maxfbar=40,plusgroup=40),
            m.spwn       = 0,
            harvest.spwn = m.spwn,
            f.year.prop = 0.5, # proportion of year when fishing happens
            T=290,
            units=if("units" %in% names(attributes(par))) attributes(par)$units else NULL,
            ...){

  # Check that m.spwn and harvest.spwn are 0 - 1
  if (m.spwn > 1 | m.spwn < 0 | harvest.spwn > 1 | harvest.spwn < 0 | f.year.prop > 1 | f.year.prop < 0)
    stop("m.spwn, harvest.spwn and f.year.prop must be in the range 0 to 1\n")
 
   age=FLQuant(range["min"]:range["max"],dimnames=list(age=range["min"]:range["max"]))
   
   # Get the lengths through different times of the year
   stocklen   <- growth(par[c("linf","t0","k")],age+m.spwn)    # stocklen is length at spawning time
   catchlen   <- growth(par[c("linf","t0","k")],age+f.year.prop) # catchlen is length when fishing happens
   midyearlen <- growth(par[c("linf","t0","k")],age+0.5) # midyear length used for natural mortality

   # Corresponding weights
   swt=par["a"]*stocklen^par["b"]
   cwt=par["a"]*catchlen^par["b"]
   if ("bg" %in% dimnames(par)$param)  
      swt=par["a"]*stocklen^par["bg"]
  
   m.spwn.      =FLQuant(m.spwn,      dimnames=list(age=range["min"]:range["max"]))
   harvest.spwn.=FLQuant(harvest.spwn,dimnames=list(age=range["min"]:range["max"]))

   m.   =fnM(  par=par,len=midyearlen,T=T) # natural mortality is always based on mid year length
   mat. =fnMat(par,age + m.spwn) # maturity is biological therefore + m.spwn
   sel. =fnSel(par,age + f.year.prop) # selectivty is fishery  based therefore + f.year.prop
 

   ## create a FLBRP object to   calculate expected equilibrium values and ref pts
   dms=dimnames(m.)
   res=FLBRP(stock.wt       =swt,
             landings.wt    =cwt,
             discards.wt    =cwt,
             bycatch.wt     =cwt,
             m              =m.,
             mat            =FLQuant(mat., dimnames=dimnames(m.)),
             landings.sel   =FLQuant(sel., dimnames=dimnames(m.)),
             discards.sel   =FLQuant(0,    dimnames=dimnames(m.)),
             bycatch.harvest=FLQuant(0,    dimnames=dimnames(m.)),
             harvest.spwn   =FLQuant(harvest.spwn,    dimnames=dimnames(m.)),
             m.spwn         =FLQuant(m.spwn,    dimnames=dimnames(m.)),
             availability   =FLQuant(1,    dimnames=dimnames(m.)))

  # units

   ## FApex
   range(res,c("minfbar","maxfbar"))[]<-as.numeric(dimnames(landings.sel(res)[landings.sel(res)==max(landings.sel(res))][1])$age)

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))[names(getSlots("FLBRP"))!="fbar"]])
     slot(res, slt)<-args[[slt]]

   params(res)=propagate(params(res),dims(res)$iter)

   ## Stock recruitment relationship
   model(res) =do.call(sr$model,list())$model
   
   if ("alpha" %in% names(sr)){
      params(res)=do.call(FLPar,sr[-seq(length(sr))[names(sr)=="model"]])
   }else{   
      set=function(model,spr0,s,v,d=NULL) FLPar(abPars(model,spr0,s,v,d))
      for (i in seq(dims(res)$iter)){
          sr$spr0=spr0(iter(res,i))
 
         iter(params(res),i)= params(res)=do.call(set,sr)
         }
       }
      
   
   dimnames(refpts(res))$refpt[5]="crash"

   res=brp(res)
   
   if ("fbar" %in% names(args)) 
       fbar(res)<-args[["fbar"]]
   else if (!is.nan(refpts(res)["crash","harvest"])) 
         fbar(res)<-FLQuant(seq(0,1,length.out=101))*refpts(res)["crash","harvest"]
  
   res=brp(res)

   if (!("units" %in% names(attributes(par))))  return(res)

    res <- setUnits(res, par)

  return(res)
  }
