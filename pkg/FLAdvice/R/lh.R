gislasim=function(par,sl=5,sr=5000){
   
  names(dimnames(par))=tolower(names(dimnames(par)))

  ## growth parameters
  if (!("t0" %in% dimnames(par)$params)) par=rbind(par,FLPar("t0"=0))
  if (!("a"  %in% dimnames(par)$params)) par=rbind(par,FLPar("a" =0.01))
  if (!("b"  %in% dimnames(par)$params)) par=rbind(par,FLPar("b" =3))
    
  if (!("k"  %in% dimnames(par)$params)) par=rbind(par,FLPar("k"=exp(0.5236+c(log(par["linf"]))*-0.4540)))
 
  ### why does rbind(FLPar,numeric) not return an FLPar
  #par=rbind(par,FLPar("sinf"=par["a"]*par["linf"]^par["b"]))

  ## maturity parameters
  par=FLPar(rbind(par,FLPar(c("a50"=0.8776*par["linf",]-0.038,"ato95"=0,"asym"=1.0))))
  par["a50"]=invVonB(par,c(par["a50"]))
  
  ## selectivity
  par=rbind(par,FLPar(a1=par["a50"],sl=sl,sr=sr))
  
  return(par)}

#### Life Hisotory Generator ####################################################
# TODO lh(gislasim(par))
# TODO Add support for FLSR
lh=function(par,
            growth       =vonB,
            fnM          =function(par,len,T=290,a=FLPar(c(-2.1104327,-1.7023068,1.5067827,0.9664798,763.5074169)))
                                    exp(a[1]+a[2]*log(len) + a[3]*log(par["linf"]) + a[4]*log(par["k"]) + a[5]/T),
#            mFn          =function(par,len) exp(0.55 - 1.61*log(len) + 1.44*log(par["linf"]) + log(par["k"])),
            fnMat        =logistic,
            selFn        =dnormal,
            sr           =list(model="bevholt",steepness=0.9,vbiomass=1e3),
            age=1:40+0.5,T=290,...){
  
   age=FLQuant(age,dimnames=list(age=floor(age)))
   len=growth(par[c("linf","t0","k")],age)
   wts=par["a"]*len^par["b"]
   
   m.   =fnM(  par=par,len=len,T=T)
   mat. =fnMat(par,age)
   sel. =selFn(par,age)

   ## create a FLBRP object to   calculate expected equilibrium values and ref pts
   dms=dimnames(m.)
   res=FLBRP(stock.wt       =wts,
             landings.wt    =wts,
             discards.wt    =wts,
             bycatch.wt     =wts,
             m              =m.,
             mat            =FLQuant(mat., dimnames=dimnames(m.)),
             landings.sel   =FLQuant(sel., dimnames=dimnames(m.)),
             discards.sel   =FLQuant(0,    dimnames=dimnames(m.)),
             bycatch.harvest=FLQuant(0,    dimnames=dimnames(m.)),
             harvest.spwn   =FLQuant(0,    dimnames=dimnames(m.)),
             m.spwn         =FLQuant(0,    dimnames=dimnames(m.)),
             availability   =FLQuant(1,    dimnames=dimnames(m.)))

   ## FApex
   range(res,c("minfbar","maxfbar"))[]<-as.numeric(dimnames(landings.sel(res)[landings.sel(res)==max(landings.sel(res))][1])$age)

   ## replace any slot passed in as an arg
   args<-list(...)
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))[names(getSlots("FLBRP"))!="fbar"]])
     slot(res, slt)<-args[[slt]]

   ## Stock recruitment relationship
   model(res) =do.call(sr$model,list())$model
   params(res)=FLPar(abPars(sr$model,spr0=spr0(res),s=sr$steepness,v=sr$vbiomass))

   dimnames(refpts(res))$refpt[5]="crash"

   res=brp(res)
   
   if ("fbar" %in% names(args)) 
       fbar(res)<-args[["fbar"]]
   else
       fbar(res)<-FLQuant(seq(0,1,length.out=101))*refpts(res)["crash","harvest"]
  
   return(brp(res))}

#ex=lh(gislasim(FLPar(linf=70)))
#ggplot(ex[[c("m","stock.wt","mat","landings.sel")]])+geom_line(aes(age,data))+facet_wrap(~qname,scale="free")
#plot(ex)
