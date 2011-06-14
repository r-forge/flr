### ======================================================================================================
### "Otolith" plot
### ======================================================================================================
plot.otolith <- function(stock, ica, plot=TRUE, show.points=FALSE, do.contours=TRUE, margin.plots=TRUE, show.estimate=TRUE,
                      xlim="missing", ylim="missing",
                      n=10000, pch=".", alpha=0.05, show.grid=TRUE,
                      n.grid=50, contour.args=list(),...){
  require(MASS)
  debug <- FALSE
  filled.contours <- FALSE

  #Start by extacting necessary data from the stock object
  last.year <-  stock@range["maxyear"]
  pgroup    <-  stock@range["plusgroup"]
  f.ages        <-  stock@range["minfbar"]:stock@range["maxfbar"]
  harvest.spwn  <-  drop(trim(stock@harvest.spwn,year=last.year)@.Data)
  m             <-  drop(trim(stock@m,year=last.year)@.Data)
  m.spwn        <-  drop(trim(stock@m.spwn,year=last.year)@.Data)
  stock.wt      <-  drop(trim(stock@stock.wt,year=last.year)@.Data)
  mat           <-  drop(trim(stock@mat,year=last.year)@.Data)
  catch.n       <-  drop(trim(ica@catch.n,year=last.year)@.Data)

  #Extract vcov matrix and param means
  param.means <-  ica@param$ICA.Output
  param.vcov  <-  ica@covar
  if(debug) param.vcov <- param.vcov*0      #Set the variance of the vcov matrix to zero, to allow debugging of code

  #Generate randomly distributed numbers based on the ENTIRE vcov matrix!!! Remember that there are covariances
  #between all parameters, even if they don't directly effect the calculation of the F and SSB
  random.params  <-  mvrnorm(n=n,param.means,param.vcov)

  #Now split the array up into the various sub-arrays
  termFs.params   <-  subset(ica@param,Param=="F")
  termF.index     <-  termFs.params[which.max(as.numeric(termFs.params$Year)),"No"]
  termF           <-  random.params[,termF.index]
  sels            <-  random.params[,subset(ica@param,Param=="Sel")$No]
  Ns              <-  random.params[,subset(ica@param,Param=="TermN")$No]

  #Can now revert back to normal space
  termF   <-  exp(termF)
  sels    <-  exp(sels)
  Ns      <-  exp(Ns)

  #Now add the selectivity of the reference age, last true age and plus group
  colnames(sels)  <-  paste("Sel",subset(ica@param,Param=="Sel")$Age)
  odd.sels  <-  matrix(c(1,ica@control@sep.sel,ica@control@sep.sel),ncol=3,nrow=n)

  colnames(odd.sels)  <-  paste("Sel",c(ica@control@sep.age,pgroup-1,pgroup))
  sels      <-  cbind(sels,odd.sels)
  sels      <-  sels[,order(colnames(sels))]

  #And now calculate Fbar
  Fage      <-  sweep(sels,1,termF,"*")
  colnames(Fage)  <-  gsub("Sel","Age",colnames(Fage))
  if(is.null(f.ages)) {
      Fbar  <-  rowMeans(Fage)
      f.ages  <-  as.numeric(dimnames(ica@harvest)$age)
  } else {
      Fbar  <-  rowMeans(Fage[,paste("Age",f.ages)])
  }

  #Next comes SSB. This is a bit trickier. First we need to calculate the number at plus group age
  z         <-  termF+m[as.character(pgroup)]
  exp.term  <-  (1-exp(-z))/z
  pgroupN   <-  catch.n[as.character(pgroup)]/termF/exp.term
  Ns        <-  cbind(Ns,pgroupN)

  #Now, having all the Ns, we can calculate the SSB
  a         <-  sweep(Fage,2,harvest.spwn,"*")
  b         <-  exp(sweep(-a,2,m*m.spwn,"-"))
  d         <-  sweep(b,2,stock.wt,"*")
  e         <-  sweep(d,2,mat,"*")
  wts.by.age  <-  e*Ns
  SSB       <-  rowSums(wts.by.age)

  #Generate the estimate from the data
  SSB.est   <-  drop(ssb(stock)[,as.character(last.year)]@.Data)
  Fbar.est  <-  mean(drop(ica@harvest[as.character(f.ages),as.character(last.year)]))

  #Set these points up to be returned
  return.obj  <- data.frame(Fbar,SSB)

  #Now calculate the contour lines, if requested
  if(do.contours) {
    #Calculate kernel density estimate
    kern  <-  kde2d(Fbar,SSB,n=n.grid)
    #Calculate cumulative distribution function
    kz    <-  as.vector(kern$z)
    ord   <-  order(kz)
    cumfrac <-  cumsum(kz[ord])/sum(kz)
    cumfrac.matrix  <-  matrix(cumfrac[rank(kz)],nrow=nrow(kern$z),ncol=ncol(kern$z))
    if(is.null(contour.args$levels)) contour.args$levels <-   c(0.01,0.05,0.25,0.5,0.75)
#      if(is.null(contour.args$lty))   contour.args$lty <-   c(1,1,2,3,4)
#      if(is.null(contour.args$lwd))   contour.args$lwd <-   c(1,3,1,1,1)
    if(is.null(contour.args$method))contour.args$method <-    "edge"
#      if(filled.contours) {
#       do.call(filled.contour,c(x=list(kern$x),y=list(kern$y),z=list(cumfrac.matrix),add=TRUE,nlevels=100,color.palette=heat.colors))
#    }
    otolith.obj  <-  c(x=list(kern$x),y=list(kern$y),z=list(cumfrac.matrix),add=TRUE,contour.args)
    return.obj    <- otolith.obj
  }

  #Now do the plot
  if(plot) {
    if(!show.points) pch <- NA
    if(margin.plots) {
      par(mar=c(0.5,0.5,0.5,0.5),oma=c(4,4,1.5,0))
      layout(matrix(c(1,4,3,2),2,2,byrow=TRUE), c(3,lcm(4)), c(lcm(4),3), respect=FALSE)
    } else { #Else force to a 1x1 plot
      par(mfrow=c(1,1))
    }

    x.lab <-  paste("Fbar (",ifelse(is.null(f.ages),"All Ages",paste(range(f.ages),collapse="-")),")",sep="")
    if(missing(xlim)) xlim  <- range(pretty(Fbar))
    if(missing(ylim)) ylim  <- range(pretty(SSB))
    #First the horizontal plot
    if(margin.plots) {
      densF   <-  density(Fbar)
      plot(densF,ann=FALSE,xaxt="n",yaxt="n",type="l",xlim=xlim)
      if(show.grid) grid()
      title(ylab="Probability\nDensity",xpd=NA,mgp=c(1,1,0))
      #Calculate 95% confidence intervals
      cumsumF.fun <-  approxfun(cumsum(densF$y)/sum(densF$y),densF$x)
      densF.fun   <-  approxfun(densF$x,densF$y)
      ul.F    <-  cumsumF.fun(1-alpha/2)
      ul.dens <-  densF.fun(ul.F)
      ll.F    <-  cumsumF.fun(alpha/2)
      ll.dens <-  densF.fun(ll.F)
      points(c(ll.F,ul.F),c(ll.dens,ul.dens),pch="|",cex=1.5)
      text(c(ll.F,ul.F),c(ll.dens,ul.dens),label=sprintf("%.3f",round(c(ll.F,ul.F),3)),pos=4,xpd=NA)
      if(show.estimate) {
        points(Fbar.est,densF.fun(Fbar.est),pch=19,cex=1.5)
        text(Fbar.est,densF.fun(Fbar.est),label=sprintf("%.3f",round(Fbar.est,3)),pos=4,xpd=NA)
      }
    }
    #Now the vertical plot
    if(margin.plots) {
      densSSB <-  density(SSB)
      plot(densSSB$y,densSSB$x,xaxt="n",yaxt="n",type="l",ylim=ylim)
      abline(v=0,col="grey")
      if(show.grid) grid()
      title(xlab="Probability\nDensity",xpd=NA,mgp=c(2,1,0))
      #Calculate 95% confidence intervals
      cumsumSSB.fun <-  approxfun(cumsum(densSSB$y)/sum(densSSB$y),densSSB$x)
      densSSB.fun   <-  approxfun(densSSB$x,densSSB$y)
      ul.SSB    <-  cumsumSSB.fun(1-alpha/2)
      ul.dens <-  densSSB.fun(ul.SSB)
      ll.SSB    <-  cumsumSSB.fun(alpha/2)
      ll.dens <-  densSSB.fun(ll.SSB)
      points(c(ll.dens,ul.dens),c(ll.SSB,ul.SSB),pch="-",cex=2)
      text(c(ll.dens,ul.dens),c(ll.SSB,ul.SSB),label=round(c(ll.SSB,ul.SSB),0),pos=4,xpd=NA)
      if(show.estimate) {
        points(densSSB.fun(SSB.est),SSB.est,pch=19,cex=1.5)
        text(densSSB.fun(SSB.est),SSB.est,,label=round(SSB.est,0),pos=2,xpd=NA)
      }
    }
    #Now the main plot
    plot(0,0,xlim=xlim,ylim=ylim,type="n",xlab="",ylab="",...)
    if(show.points) points(Fbar,SSB,pch=pch,...)
    title(xlab=x.lab,ylab="SSB",xpd=NA)
    if(show.estimate) points(Fbar.est,SSB.est,pch=19,cex=1.5)
    if(show.grid) grid()
    if(do.contours) {
      do.call(contour,otolith.obj)
    }

  }

  return(invisible(return.obj))
}


### ======================================================================================================
### "Banana" plot - simply a wrapper to the otolith plot
### ======================================================================================================
plot.banana <- function(stock,ica,show.points=TRUE,do.contours=FALSE,margin.plots=FALSE,...){
  opt <-  plot.otolith(stock,ica,show.points=show.points,do.contours=do.contours,margin.plots=margin.plots,...)
  return(invisible(opt))
}
