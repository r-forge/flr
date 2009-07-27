diagResidPlot<-function(yrs,hat,indVar,indVar.,prd,obs,resid,xttl="X",yttl="Y",mttl="")
  	{
    # initial device settings
    trellis.device(new=FALSE)
    trellis.par.set(list(layout.heights = list(bottom.padding = -0.5,
      axis.xlab.padding = 0.5, xlab = -0.5), layout.widths = list(left.padding = -0.5,
      right.padding = -0.5, ylab.axis.padding = -0.5)))

		# panel functions
		srpanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='gray40', cex=0.8)
			panel.loess(x,y, col='red')
			panel.abline(a=0, b=0, lty=2, col='blue')
  		}

		respanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='gray40', cex=0.8)
      panel.lmline(x, y, ..., col='red')
			panel.abline(a=0, b=0, lty=2, col='blue')
		  }
		# get dimensions to condition on (skip quant)
		condnames <- names(hat)[c(3:5)][dim(hat)[c(3:5)]!=1]
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)
		# 1. SR values with fitted curve
    print(xyplot(formula(paste("hat~indVar", cond)), ylab=yttl, xlab=xttl,
			model.frame(FLQuants(obs=obs, indVar=indVar., hat=prd)),
      col='red', main=mttl,
      xlim=c(0, max(ssb, na.rm=TRUE)), ylim=c(0, max(obs, na.rm=TRUE)+
      (max(obs,na.rm=TRUE)/10)), groups=iter, type='l'), split=c(1,1,2,3), more=TRUE)

		# Add model line
		# TODO Model line by unit/area/season, if params are so too
		trellis.focus("panel", 1, 1)
    lpoints(indVar, obs, col='black', cex=0.8)
    llines(lowess(obs~indVar), col='blue',  cex=0.8)
		trellis.unfocus()

		# 2. Residuals plotted against year
		print(xyplot(formula(paste("resid~year", cond)), ylab='Residuals', xlab='',
			data=model.frame(FLQuants(resid=resid)),
			panel=srpanel, main='Residuals by year'), split=c(2,1,2,3), more=TRUE)

		# 3. Residuals at time t vs. residuals at time t+1
		print(xyplot(formula(paste("resid1~resid", cond)), ylab='Residuals at t+1',
      xlab='Residuals at t', model.frame(FLQuants(resid=resid,
      resid1=FLQuant(resid,
      dimnames=list(year=as.numeric(dimnames(resid)$year)+1)))),
		  panel=respanel, main='AR(1) Residuals'), split=c(1,2,2,3), more=TRUE)

		# 4. Residuals plotted against independent variable
		print(xyplot(formula(paste("resid~ssb", cond)), ylab='Residuals', xlab='SSB',
			model.frame(FLQuants(resid=resid, ssb=indVar)),
			panel=srpanel, main='Residuals by SSB'), split=c(2,2,2,3), more=TRUE)

		# 5. Residuals plotted against observed
		print(xyplot(formula(paste("resid~fitted", cond)), ylab='Residuals', xlab='R hat',
			model.frame(FLQuants(resid=resid, fitted=hat)),
			panel=srpanel, main='Residuals by Estimated Recruits'), split=c(1,3,2,3),
			more=TRUE)

		# 6. qqplot of residuals
		print(qqmath(formula(paste("~resid", cond)), ylab='Residuals',
    xlab='Sample Quantiles', model.frame(FLQuants(resid=resid)),
      panel = function(x, ...) {
          panel.qqmath(x, ..., , col='gray40', cex=0.8)
          panel.qqmathline(x, ..., col='red')
       }, main='Normal Q-Q Plot'), split=c(2,3,2,3), more=FALSE)
		invisible()
	  }
	

