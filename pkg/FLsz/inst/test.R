library(FLCore)
library(ggplotFL)

data(ple4)

source("/home/lkell/flr/pkg/FLsz/R/class.R")
source("/home/lkell/flr/pkg/FLsz/R/allGenericMethods.R")
source("/home/lkell/flr/pkg/FLsz/R/createAccessors.R")
source("/home/lkell/flr/pkg/FLsz/R/constructors.R")
source("/home/lkell/flr/pkg/FLsz/R/coercion.R")
source("/home/lkell/flr/pkg/FLsz/R/plots.R")
source("/home/lkell/flr/pkg/FLsz/R/methods.R")
source("/home/lkell/flr/pkg/FLsz/R/growth.R")
source("/home/lkell/flr/pkg/FLsz/R/indicators.R")

grw=FLPar(a=1,b=3,Linf=100,t0=0,k=.4,lc=10)
     
obj1=FLsz(ple4,grw=grw)

obj2=FLsz(obs=apply(wt2len(grw,stock.wt(ple4))*stock.n(ple4),2:6,sum)/apply(stock.n(ple4),2:6,sum),
          grw=grw)

