#### Diagnostics
## Diagnostics
setGeneric('plotDiags', function(object, ...)
	standardGeneric('plotDiags'))

setMethod("plotDiags", signature(object="glm"),
   function(object){

   vplayout <-function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

   smry<-data.frame(resStd    =rstandard(object),
                    res       =object$residuals,
                    hatLn     =object$linear.predictors,
                    hat       =object$fitted.values,
                    y         =object$y)

  grid.newpage()
  pushViewport(viewport(layout=grid.layout(2,2)))

  rsdl<-qqnorm(rstandard(object),plot.it=FALSE)
  rsdl<-data.frame(x=rsdl$x,y=rsdl$y)

  p<-ggplot(rsdl) + geom_point(aes(x,y),size=0.5)   +
                    opts(title = "Normal Q-Q Plot") + scale_x_continuous(name="Theoretical Quantiles") +
                                                      scale_y_continuous(name="Sample Quantiles")  +
                    geom_abline(intercept=0, slope=1)
  print(p, vp=vplayout(1,1))

  p<-ggplot(smry) +
                geom_point(aes(hat,resStd),size=0.5) + stat_smooth(aes(hat,resStd),method="gam") +
                opts(title="Error Distributions")    + scale_x_continuous(name="Predicted") +
                                                       scale_y_continuous(name="Standardised Residuals")
  print(p, vp=vplayout(1,2))

  p<-ggplot(smry) +
                geom_point(aes(hatLn,res), size=0.5) + stat_smooth(aes(hatLn,res),method="gam") +
                opts(title="Assumed Variance") + scale_x_continuous(name="Predicted on Link") +
                                                 scale_y_continuous(name="Absolute Residuals")
  print(p, vp=vplayout(2,1))
                                                                                                                       cari
  p<-ggplot(smry) +
                geom_point(aes(hatLn,y), size=0.5) + stat_smooth(aes(hatLn,y),method="gam") +
                opts(title="Link Function") + scale_x_continuous(name="Predicted on Link") +
                                              scale_y_continuous(name="Observed")
  print(p, vp=vplayout(2,2))})

################################################################################


#### Get LSMeans for delta-
lsmeansDelta<-function(x,xBin,newdat){

  prd    <-data.frame(newdat, predict(x,    newdat,se.fit = TRUE, type="response"))
  prdBin <-data.frame(newdat, predict(xBin, newdat,se.fit = TRUE, type="response"))

  prd<-data.frame(year=sort(unique(newdat$year)),p    =with(prdBin,aggregate(fit,     list(year=year),mean))[,2],
                                                 pCv  =with(prdBin,aggregate(se.fit^2,list(year=year),mean))[,2]^.5,
                                                 index=with(prd,   aggregate(fit,     list(year=year),mean))[,2],
                                                 cv   =with(prd,   aggregate(se.fit^2,list(year=year),mean))[,2]^.5)
  prd$index <-log(exp(prd$index)*prd$p)
  prd$low   <-prd$index*(1-2*prd$cv)
  prd$upper <-prd$index*(1+2*prd$cv)

  return(prd)}
