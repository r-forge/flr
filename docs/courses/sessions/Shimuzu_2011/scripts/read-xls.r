
## reading data from xls files

library(FLCore)
library(gdata)


path        <- '/home/rob/Dropbox/FLRbook/Data/inputs/VPASuite/'
file        <- paste(path,'swordfish.xls',sep='')

readFLStockxls <- function(file) {

sheetnames  <- sheetNames(file)

yeardims <- read.xls(file, sheet=1)[2,]
agedims  <- read.xls(file, sheet=1)[3,]

FLStock. <- FLStock(catch.n=FLQuant(NA, 
                       dimnames=list(age =ac(agedims[1,1]:agedims[1,2]),
                                     year=ac(yeardims[1,1]:yeardims[1,2]),
                                     unit='unique',season='all',area='unique')))

for(name in sheetnames){
  id  <- read.xls(file, sheet=name)[1,2]

  if(is.element(id, c(1,21,24)))
    dat <- FLQuant(read.xls(file, sheet=name, skip=4)[,1],
                   dimnames=list(year=ac(yeardims[1,1]:yeardims[1,2])))  

  if(!is.element(id, c(1,21,24)))
    dat <- FLQuant(t(as.matrix(read.xls(file, sheet=name, skip=4))),
                   dimnames=list(year=ac(yeardims[1,1]:yeardims[1,2]),
                                 age =ac(agedims[1,1]:agedims[1,2]))) 

  switch(as.character(id),
    '1'  = slot(FLStock., 'landings')    <- dat,
    '2'  = slot(FLStock., 'landings.n')  <- dat,
    '3'  = slot(FLStock., 'landings.wt') <- dat,
    '4'  = slot(FLStock., 'stock.wt')    <- dat,
    '5'  = slot(FLStock., 'm')           <- dat,
    '6'  = slot(FLStock., 'mat')         <- dat,
    '7'  = slot(FLStock., 'f.spwn')      <- dat,   #change to harvest.spwn
    '8'  = slot(FLStock., 'm.spwn')      <- dat,
    '21' = slot(FLStock., 'discards')    <- dat,
    '22' = slot(FLStock., 'discards.n')  <- dat,
    '23' = slot(FLStock., 'discards.wt') <- dat,
    '24' = slot(FLStock., 'catch')       <- dat,
    '25' = slot(FLStock., 'catch.wt')    <- dat,
    '26' = slot(FLStock., 'catch.n')     <- dat,
    '27' = slot(FLStock., 'harvest')     <- dat,
    '28' = slot(FLStock., 'stock.n')     <- dat)

  }
  return(FLStock.) 
}
  
