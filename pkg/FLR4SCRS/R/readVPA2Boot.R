########################################################################################################################
# Bluefin Projections for Cites                                                                                        #
# 23rd October 2009                                                                                                    #
# L.T. Kell                                                                                                            #
#                                                                                                                      #
# Functions                                                                                                            #
########################################################################################################################

## reads in data from VPA2Box files
readVPA2Boot<-function(x,wt,mat,its=501,harvest.spwn=0.5,m.spwn=0.5,
            units=list("Tonnes","1s","Kg","Tonnes","1s","Kg","Tonnes","1s","Kg","Tonnes","1s","Kg","","Proportion","f","Proportion","Proportion")){

      ## get binary file
      getBoot<-function(con,ages,yrs,its){
         dmns<-list(year=yrs,age=ages,unit="unique",season="all",area="unique",iter=its)
         res <-FLQuant(aperm(array(readBin(con=con,what=double(),size=4,length(ages)*length(yrs)*length(its)),lapply(dmns,length),dmns),c(2,1,3,4,5,6)))

         return(res)}

      ages=dimnames(wt)$age
      yrs =dimnames(wt)$year
      
      if (!is(mat, "FLQuant")) mat=FLQuant(mat,dimnames=list(age=ages,year=yrs))

      ## get stock data
      stk<-FLStock(m           =iter(getBoot(paste(x,"/MAA.out",sep=""),ages,yrs,its),1),
                   harvest     =     getBoot(paste(x,"/FAA.out",sep=""),ages,yrs,its),
                   stock.n     =     getBoot(paste(x,"/NAA.out",sep=""),ages,yrs,its),
                   landings.n  =     getBoot(paste(x,"/CAA.out",sep=""),ages,yrs,its),
                   stock.wt    =wt,
                   landings.wt =wt,
                   discards.n  =FLQuant(0.0,dimnames=list(age=ages,year=yrs)),
                   discards.wt =FLQuant(0.0,dimnames=list(age=ages,year=yrs)),
                   mat         =mat,
                   harvest.spwn=FLQuant(harvest.spwn,dimnames=list(age=ages,year=yrs)),
                   m.spwn      =FLQuant(m.spwn,dimnames=list(age=ages,year=yrs)))

      catch(   stk)<-computeCatch(   stk,slot="all")
      landings(stk)<-computeLandings(stk)
      discards(stk)<-computeDiscards(stk)

      units(stk)[1:17]<-units

      return(stk)}

