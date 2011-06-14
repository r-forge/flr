biol <-as(ple4,"FLBiol")
flt  <-bt4
mtr  <-bt4[[1]]
ctch <-bt4[[1]][[1]]

computeHarvest(ple4)

#### FLBiol & FLQuant
harvest(biol,catch.n(ctch))

#### FLBiol & FLCatch
harvest(biol,ctch)

#### FLBiol & FLMetier
harvest(biol,mtr,"ple")

#### FLBiol & FLFleet
harvest(biol,mtr,1)

#### FLBiol & FLFleet
harvest(biol,flt,1,1)
