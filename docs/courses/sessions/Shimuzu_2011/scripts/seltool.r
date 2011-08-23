
# an extensible tool to investigate selection properties

library(FLCore)
library(lattice)

popsel <- function(fmult=c(0.4,0.2,0.1), rcts=rep(1000,3), mix=0.05, symmetric=T){

  # define the dimensions
  ages  <- 1:15
  areas <- 1:length(fmult)

  # define static parameters
  nm    <- 0.2
  a50   <- 4
  b     <- 1.5
  sel   <- FLQuant(1/(1+exp(-b*(ages-a50))),dimnames=list(age=ages))

  # movement matrix
  mixmat <- array(mix, dim=c(length(areas),length(areas)))
  if(symmetric)
    diag(mixmat) <- 1-(mix*(length(areas)-1))

  if(!symmetric){
    mixmat <- diag(c(rep(1-mix,length(fmult)-1),1))
    mixmat[lower.tri(mixmat)][cumsum(1:(length(fmult)-1))] <- mix
  }
  # calculate N at age
  Ns <- FLQuant(NA, dimnames=list(age=ac(ages), area=ac(areas)))
  Ns[1,] <- rcts
  for(j in ages[-min(ages)]) {
    for(k in areas) {    
      Ns[j,,,,k,] <-sum(Ns[j-1,,,,,] * mixmat[k,] * exp(-nm -fmult*c(sel[j-1])))
    }
  }

  # population selection
  popF   <- log(areaSums(Ns)[-max(ages)]/areaSums(Ns)[-min(ages)])-nm
  popsel <- popF/max(popF)
  return(FLQuants(popsel=popsel, gearsel=sel[-max(ages)]))
}

xyplot(data~age, groups=qname, 
       data=popsel(fmult=c(0.4 ,0.2 ,0.1),rcts =c(1000,1000,1000), mix=0.05), type='l')


pfun <- function(x,y,labx,laby,lab,...){
  panel.xyplot(x,y,...)
  panel.text(labx, laby, ac(lab), cex=0.8)
}

# scenario 1 : different levels of mixing - symmetric movement, equal recruitment
mixvals <- c(0.0, 0.05, 0.1, 0.15)
res     <- propagate(popsel(mix=mixvals[1])$popsel, length(mixvals))
for(i in 2:length(mixvals))
  iter(res,i) <- popsel(mix=mixvals[i])$popsel

xyplot(data~age, groups=iter, data=res, type='l', col='black', panel=pfun, 
       labx=rep(15,3), laby=c(res[14,]), lab=mixvals, xlim=c(1,16),
       main='Symmetric Movement, Equal Recmnt, Different Mixing', xlab='Age', ylab='Sel')

# scenario 2 : mixing 0.05, symmetric movement, unequal recruitment
recratios =c(1, 0.8, 0.6, 0.4, 0.2, 0.0)
res <- propagate(popsel(rcts=c(1000,1000,1000))$popsel, length(recratios))
for(i in 2:length(recratios))
  iter(res,i) <- popsel(rcts=c(1000, 1000*recratios[i], 1000*recratios[i]^2))$popsel

xyplot(data~age, groups=iter, data=res, type='l', col='black', panel=pfun, 
       labx=15, laby=c(res[14,]), lab=recratios, xlim=c(1,16),
       main='Symmetric Movement, Different Recmnt, Mixing=0.05', xlab='Age', ylab='Sel')

# scenario 3 : mixing 0.05, symmetric movement, equal rectmnt, different F ratios
fratios <- c(0.5, 1.0, 0.7, 0.6, 0.4, 0.3, 0.2)
res <- propagate(popsel(fmult=c(0.4, 0.2, 0.1))$popsel, length(fratios))
for(i in 2:length(fratios))
  iter(res, i) <- popsel(fmult=c(0.4, 0.4*fratios[i], 0.4*fratios[i]^2))$popsel

xyplot(data~age, groups=iter, data=res, type='l', col='black', panel=pfun, 
       labx=15, laby=c(res[14,]), lab=fratios, xlim=c(1,16),
       main='Symmetric Movement, Equal Recmnt, Mixing=0.05, Diff Fs', xlab='Age', ylab='Sel')

# scenario 4 : assymetric movement, mixing 0.05, equal rectmnt, different F ratios
fratios <- c(1.0, 0.5, 0.6, 0.4)
res <- propagate(popsel(fmult=c(0.4, 0.4, 0.4),symmetric=F)$popsel, length(fratios))
for(i in 2:length(fratios))
  iter(res, i) <- popsel(fmult=c(0.4, 0.4*fratios[i], 0.4*fratios[i]^2),symmetric=F)$popsel

xyplot(data~age, groups=iter, data=res, type='l', col='black', panel=pfun, 
       labx=15, laby=c(res[14,]), lab=fratios, xlim=c(1,16),
       main='NonSymmetric Movement, Equal Recmnt, Mixing=0.05, Diff Fs', xlab='Age', ylab='Sel')

# scenario 5 : assymetric movement, mixing 0.05, unequal rectmnt, same F ratios
recratios <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0.0)
res <- propagate(popsel(rcts=c(1000, 1000, 1000))$popsel, length(recratios))
for(i in 2:length(recratios))
  iter(res, i) <- popsel(rcts=c(1000, 1000*recratios[i], 1000*recratios[i]^2),symmetric=F)$popsel

xyplot(data~age, groups=iter, data=res, type='l', panel=pfun, 
       labx=15, laby=c(res[14,]), lab=recratios, xlim=c(1,16),
       main='NonSymmetric Movement, UnEqual Recmnt, Mixing=0.05, Same Fs', 
       xlab='Age', ylab='Sel', col=c(rep('black',length(recratios)-1),'red'))





#
#



