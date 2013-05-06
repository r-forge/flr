################################################################################
#                                                                              #
# Methods to create FLR objects from MFCL                                      #
#                                                                              #
################################################################################

getMassAge <- function(parfile = getoutputparfile("plot.rep"),inifile,a) {
   gp<-getGrowthPars(parfile)
   ln<-gp$size1+(gp$Linf-gp$size1)*((1-exp(-gp$rho*(a-1)))/(1-exp(-gp$rho*(1000-1))))
   lw<-getLnWt(inifile)

   lw[1]*ln^lw[2]
   }


mfclFLStock<-function(fileRep,filePar,timeFish=0.5){
#      getGrowthPars(filePar)

      nreg<-getnreg(fileRep)
#      if (nreg!=1)
#         stop("Currently only checked for 1 FLR area and MF-CL region")

      if (getnpd( fileRep)!=getnyr(fileRep))
         nseason<-getnpd(fileRep)/getnyr(fileRep) else
         nseason<-1

      minage<-1
      minyr <-getyear1(fileRep)


      if (nseason>1) dmSn=1:nseason else dmSn="all"
      if (nreg   >1) dmAr=1:nreg    else dmAr="unique"

      dmns=list(age=minage:getnages(fileRep)+minage-1, year=minyr:(minyr+getnyr(fileRep)-1), unit="unique", season=dmSn, area=dmAr)

      wt<-FLQuant(getwt.age(fileRep), dimnames=dmns)

      stk<-FLStock(catch       =FLQuant(NA,                    dimnames=dmns[-1]),
                   catch.n     =FLQuant(NA,                    dimnames=dmns),
                   catch.wt    =wt,
                   discards    =FLQuant(0,                     dimnames=dmns[-1]),
                   discards.n  =FLQuant(0,                     dimnames=dmns),
                   discards.wt =wt,
                   landings    =FLQuant(NA,                    dimnames=dmns[-1]),
                   landings.n  =FLQuant(NA,                    dimnames=dmns),
                   landings.wt =wt,
                   stock       =FLQuant(NA,                    dimnames=dmns[-1]),
                   stock.wt    =wt,
                   harvest     =FLQuant(NA,                    dimnames=dmns, units="f"),
                   m           =FLQuant(getM.age(   fileRep)/nseason,  dimnames=dmns),
                   mat         =FLQuant(getmaturity(filePar),  dimnames=dmns),
                   harvest.spwn=FLQuant(0,                     dimnames=dmns),
                   m.spwn      =FLQuant(0,                     dimnames=dmns),
                   name        ="",
                   desc        ="read in from Multifan-CL")

      N           <-getNya(fileRep)
      N           <-array(c(N),c(nseason,length(dmns$year),length(dmns$age),nreg,1,1))
      stock.n(stk)<-FLQuant(aperm(N,c(3,2,5,1,4,6)),dimnames=dmns)

      f           <-getFya(fileRep)
      f           <-array(c(f),c(nseason,length(dmns$year),length(dmns$age),nreg,1,1))
      harvest(stk)<-FLQuant(aperm(f,c(3,2,5,1,4,6)),dimnames=dmns, units="f")

      catch.n(stk)   <-stock.n(stk)*harvest(stk)/(harvest(stk)+m(stk))*(1-exp(-harvest(stk)-m(stk)))
      landings.n(stk)<-catch.n(        stk)
      catch(   stk)  <-computeCatch(   stk,"all")
      landings(stk)  <-computeLandings(stk)
      discards(stk)  <-computeDiscards(stk)
      stock(   stk)  <-computeStock(   stk)

      return(stk)
      }

readFLStock <- function (file, file2="NULL",type = "VPA", name, desc = paste("Imported from a",
    type, "file. (", file, "). ", date()), m = 0.2, quant="age", quiet=TRUE, no.discards=FALSE,timeOfFish=0.5)
 	  {
	  if (type!="mfcl")
       return(FLCore::readFLStock(file,type,name,desc,m,quant,quiet,no.discards)) else
       return(mfclFLStock(fileRep=file,filePar=file2,timeOfFish))
    }

#### FLBiol ####################################################################
mfclFLBiol<-function(fileRep,filePar){

    biol<-as(mfclFLStock(fileRep,filePar),"FLBiol")

    return(biol)
    }

#### FLCatches #################################################################
mfclFLCatches<-function(fileRep){
    if ( getnreg(fileRep)!=1)               stop("Currently only checked for 1 FLR area and MF-CL region")

    minage<-1
    minyr <-floor(min(unlist(getrtimes(fileRep))))

    ctchs<-FLCatches()
    eff  <-FLQuants()

    for (i in 1:getnfish(fileRep)){
       if ((getnpd( fileRep)!=getnyr(fileRep)))
          seasons<-sort(unique(getrtimes(fileRep)[[1]]-as.integer(getrtimes(fileRep)[[1]])))
       else
          seasons<-1
       dmns=list(age=minage:getnages(fileRep)+minage-1, year=minyr:(minyr+getnyr(fileRep)-1), unit=1, season=1:length(seasons), area=1:getnreg(fileRep))

        yrs    <-sort(unique(as.integer(getrtimes(fileRep)[[1]])))

        minage<-1
        dmns=list(age=minage:getnages(fileRep)+minage-1, year=yrs, unit=1, season=seasons, area=1:getnreg(fileRep))

        catchability<-getqedlist(fileRep)[[i]]+getqlist(fileRep)[[i]]

        ctchs[[i]]<-FLCatch(landings     =FLQuant(NA,                     dimnames=dmns[-1]),
                            landings.n   =FLQuant(getColist(fileRep)[[i]],dimnames=dmns),
                            landings.wt  =FLQuant(getwt.age(  fileRep),   dimnames=dmns),
                            landings.sel =FLQuant(getselect(fileRep)[i,], dimnames=dmns),
                            discards     =FLQuant(NA,                     dimnames=dmns[-1]),
                            discards.n   =FLQuant(0,                      dimnames=dmns),
                            discards.wt  =FLQuant(getwt.age(  fileRep),   dimnames=dmns),
                            discards.sel =FLQuant(0,                      dimnames=dmns),
                            catch.q      =FLQuant(catchability,           dimnames=dmns),
                            price        =FLQuant(NA,                     dimnames=dmns),
                            name         ="",
                            desc         ="read in from Multifan-CL")

              landings(ctchs[[i]])<-computeLandings(ctchs[[i]])
              discards(ctchs[[i]])<-computeDiscards(ctchs[[i]])

        eff[[i]]<-FLQuant(getColist(fileRep)[[i]]/getCPUEolist(fileRep)[[i]],dimnames=dmns[-1])
        }
        
    return(list(FLCatches=ctchs,effort=eff))
    }

#      getGrowthPars(filePar)

growth<-function(age,maxage,size1,size2,rho,Linf,t0){
    res=size1+(Linf-size1)*((1-exp(-rho*(age-1)))/(1-exp(-rho*(maxage-1))))
    }

#### FLBiol ####################################################################
mfclFLBRP<-function(fileRep,filePar){

    burp<-FLBRP(mfclFLStock(fileRep,filePar))

    model( burp)<-bevholt()$model
    params(burp)<-FLPar(c(getBHSR(fileRep)$alpha,getBHSR(fileRep)$beta))

    return(burp)
    }
