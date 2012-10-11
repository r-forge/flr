
k2smFn2=function(x,image  =list(levels=seq(0.0,1.0,0.05),
                                col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
                   contour=list(levels=c(.6,.7,1.0,.9),
                              col   =c("black")),
                   nIterp=101){

  x=subset(x, !is.na(x[,1]) & !is.na(x[,2]) & !is.na(x[,3]))
  
  ##### smooth
  t.<-akima::interp(x[,1],x[,2],x[,3],
                    xo=seq(min(x[,1]),   max(x[,1]), length=nIterp),
                    yo=seq(min(x[,2]),   max(x[,2]), length=nIterp),
                    duplicate="mean")
  

  cbind(expand.grid(x=t.$x,y=t.$y),z=cut(t.$z,image$levels,include.lowest=T),w=c(t.$z))
  dat}

k2smFn3=function(x)  t(tapply(x[,3],x[,c(1,2)],mean))


