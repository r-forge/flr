tables       <-as.list(c("obs","hat","par","age","cost","smry","srr","refpts","units"))
names(tables)<-tables

tables[["obs"]]<-c("fbar.obs",
                   "catch.obs",
                   "landings.obs",
                   "discards.obs",
                   "rec.obs",
                   "ssb.obs",
                   "stock.obs",
                   "profit.obs")

tables[["hat"]]<-c("fbar",
                   "catch.hat",
                   "landings.hat",
                   "discards.hat",
                   "rec.hat",
                   "ssb.hat",
                   "stock.hat",
                   "profit.hat")

tables[["par"]]<-c("landings.sel",
                   "discards.sel",
                   "bycatch.harvest",
                   "stock.wt",
                   "catch.wt",
                   "landings.wt",
                   "discards.wt",
                   "bycatch.wt",
                   "m",
                   "mat",
                   "harvest.spwn",
                   "m.spwn",
                   "availability",
                   "price")
                      
tables[["age"]]<-c("landings.n",
                   "discards.n",
                   "stock.n",
                   "catch.n",
                   "bycatch.n",
                   "harvest")

tables[["cost"]]<-c("vcost",
                    "fcost")

tables[["smry"]]<-c("name",
                    "desc",
                    "min",
                    "max",
                    "plusgroup",
                    "minfbar",
                    "maxfbar",
                    "quant",
                    "age",
                    "min",
                    "max",
                    "year",
                    "minyear",
                    "maxyear",
                    "plusgroup",
                    "unit",
                    "season",
                    "area",
                    "iter",
                    "sr")

tables[["units"]]<-c("fbar.obs",
                     "landings.obs",
                     "discards.obs",
                     "rec.obs",
                     "ssb.obs",
                     "stock.obs",
                     "profit.obs",
                     "fbar",
                     "landings.sel",
                     "discards.sel",
                     "bycatch.harvest",
                     "stock.wt",
#                     "catch.wt",
                     "landings.wt",
                     "discards.wt",
                     "bycatch.wt",
                     "m",
                     "mat",
                     "harvest.spwn",
                     "m.spwn",
                     "availability",
                     "price",
#                     "landings.n",
#                     "discards.n",
#                     "stock.n",
#                     "catch.n",
#                     "bycatch.n",
#                     "harvest",
                     "vcost",
                     "fcost")

x<-FLBRP(ple4)

FLBRP2DF<-function(x){
## Creates a list of data.frames etc. of FLBRP
        DF<-as.list(c("obs","hat","par","age","cost","smry","srr","refpts","units"))

        DF[["obs"]]<-model.frame(FLQuants(fbar.obs    =fbar.obs(x),
                                          catch.obs   =catch.obs(x),
                                          landings.obs=landings.obs(x),
                                          discards.obs=discards.obs(x),
                                          rec.obs     =rec.obs(x),
                                          ssb.obs     =ssb.obs(x),
                                          stock.obs   =stock.obs(x),
                                          profit.obs  =profit.obs(x)))

        DF[["hat"]]<-model.frame(FLQuants(fbar        =fbar(x),
                                          catch.hat   =catch.hat(x),
                                          landings.hat=landings.hat(x),
                                          discards.hat=discards.hat(x),
                                          rec.hat     =rec.hat(x),
                                          ssb.hat     =ssb.hat(x),
                                          stock.hat   =stock.hat(x),
                                          profit.hat  =profit.hat(x)))

        DF[["par"]]<-model.frame(FLQuants(landings.sel    =landings.sel(x),
                                          discards.sel    =discards.sel(x),
                                          bycatch.harvest =bycatch.harvest(x),
                                          stock.wt        =stock.wt(x),
                                          catch.wt        =catch.wt(x),
                                          landings.wt     =landings.wt(x),
                                          discards.wt     =discards.wt(x),
                                          bycatch.wt      =bycatch.wt(x),
                                          m               =m(x),
                                          mat             =mat(x),
                                          harvest.spwn    =harvest.spwn(x),
                                          m.spwn          =m.spwn(x),
                                          availability    =availability(x),
                                          price           =price(x)))

        DF[["age"]]<-model.frame(FLQuants(landings.n=landings.n(x),
                                          discards.n=discards.n(x),
                                          stock.n   =stock.n(x),
                                          catch.n   =catch.n(x),
                                          bycatch.n =bycatch.n(x),
                                          harvest   =harvest(x)))

        DF[["cost"]]<-model.frame(FLQuants(vcost=vcost(x),
                                           fcost=fcost(x)))

        tables[["smry"]]<-c(name=name(x),
                            desc=desc(x),
                            range(x),
                            unlist(dims(x)),
                            model(x))

        DF[["srr"]]<-as.data.frame(params(x))

        DF[["refpts"]]<-as.data.frame(params(x))

        DF[["units"]]<-c(FLQuants)

        DF[["units"]]<-lapply(i, function(x) units(do.call(x, list(pleBRP))))

        names(DF[["units"]])<-tables[["units"]]

        return(DF)}
        
DF2FLBRP<-function(x){
## Creates an FLBRP from Data.frame
  }
