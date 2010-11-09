  #### Maturity data
  fec       <-as.data.frame(mat(ple4[[2]][,,"female"]))[,c("age","year","data")]
  fec       <-fec[!is.na(fec$data),]
  fec$cohort<-fec$year-fec$age

  ## Fit single von B to all
  fecPar     <-coefficients(nls(data~Asym*exp(-b2*b3^log(age)),data=fec,start=list(Asym=2.0e5,b2=1000.0,b3=0.01)))
  fec        <-merge(fec,data.frame(age=age,hat=fecPar["Asym"]*exp(-fecPar["b2"]*fecPar["b3"]^log(age))),all=T)
  fec$residual<-log(fec$data/fec$hat)

  #### Run ADMB ##################################################################
  #setwd("C:/Stuff/FLR/WorkInProgress/rndWlk")

  ## data
  #cat(paste(dim(t(as.matrix(mat(ple4[[2]])[-1,,"female",drop=T])))[1],"\n"),file="gompertz.dat",append=FALSE)
  #cat(paste(2,15,"\n"),                                                     file="gompertz.dat",append=TRUE)
  #cat(paste(0.01,0.01,0.01,"\n"),                                           file="gompertz.dat",append=TRUE)
  #cat( t(as.matrix(mat(ple4[[2]])[-1,,"female",drop=T])),                   file="gompertz.dat",append=TRUE, label=FALSE)

  ##run
  #shell("gompertz")

  ## read
  dat <-admbPar("C:/Stuff/FLR/WorkInProgress/rndWlk/gSmpl.par")

  ggplot(rbind(data.frame(Cohort=1:73,Parameter="Asym",data=cumprod(exp(dat$Asym_devs))*dat$Asym)[c(-1,-74),],
               data.frame(Cohort=1:73,Parameter="b2",  data=cumprod(exp(dat$b2_devs))*dat$b2)[c(-1,-74),],
               data.frame(Cohort=1:73,Parameter="b3",  data=cumprod(exp(dat$b3_devs))*dat$b3)[c(-1,-74),]))     +
     geom_line(aes(Cohort,data))+facet_wrap(~Parameter,scale="free")

  fecPars<-cbind(cohort=dimnames(FLCohort(mat(ple4[[2]])[-(1:2),,"female"]))$cohort[-1],
             data.frame(Asym=cumprod(exp(dat$Asym_devs))*dat$Asym,
                        b2  =cumprod(exp(dat$b2_devs))*dat$b2,
                        b3  =cumprod(exp(dat$b3_devs))*dat$b3))

  fec          <-merge(fec,fecPars,all=T,by="cohort")
  fec$hat2     <-with(fec,Asym*exp(-b2*b3^log(age)))
  fec$residual2<-log(fec$data/fec$hat2)

  fec          <-ddply(fec,"age",transform, residual =residual/sd(residual,na.rm=T))
  fec          <-ddply(fec,"age",transform, residual2=residual2/sd(residual2,na.rm=T))

  object               =fec[fec$data>0,c("age","year","cohort","data","hat","residual","hat2","residual2")]
  names(object)[c(1,4)]=c("x","obs")

  object=ddply(object,"cohort",transform,residual=residual/sd(residual,na.rm=T))
  object=ddply(object[object$cohort %in% 1940:2004,],"cohort",QQAR)

    stuff=facet_wrap(~cohort,scale="free")
    group=NULL
    p    =ggplot(object)

    ## fit
    p1=p + opts(title = "Fit")                   +
           geom_point(aes(x,obs, group=cohort,col=cohort))+
           geom_line( aes(x,hat,group=cohort,col=cohort)) +
           geom_line( aes(x,hat2,group=cohort,col=cohort),size=2)
  p1
  p + opts(title = "Residuals")                              +
         ylab("Residuals")                                   +
         xlab("Independent Variable")                        +
         geom_point( aes(x,residual,group=group,col=group))  +
         stat_smooth(aes(x,residual,group=group,col=group))  +
         geom_abline(intercept=0, slope=0)

  p + opts(title = "Residuals")                              +
         ylab("Residuals")                                   +
         xlab("Independent Variable")                        +
         geom_point( aes(x,residual2,group=group,col=group))  +
         stat_smooth(aes(x,residual2,group=group,col=group))  +
         geom_abline(intercept=0, slope=0)

  ## qq plot
  p2=p + opts(title = "Normal Q-Q Plot")                                +
         scale_x_continuous(name="Theoretical Quantiles", scale="free") +
         scale_y_continuous(name="Sample Quantiles"     , scale="free") +
         geom_point(aes(qqx,qqy,  group=group,col=group))               +
         geom_line( aes(qqx,qqHat,group=group,col=group))

  ## Residuals v x
  p3=p + opts(title = "AR(1) Plot")                           +
         ylab("Residuals")                                   +
         xlab("Independent Variable")                        +
         geom_point( aes(x,residual,group=group,col=group)) +
         stat_smooth(aes(x,residual,group=group,col=group)) +
         geom_abline(intercept=0, slope=0)

  ## Residuals v fitted
  p4=p + opts(title = "AR(1) Plot")                            +
         geom_point( aes(hat,residual,group=group,col=group)) +
         stat_smooth(aes(hat,residual,group=group,col=group)) +
         ylab("Residuals")                                     +
         xlab("Dependent Variable")                            +
         geom_abline(intercept=0, slope=0)

  ## AR1
  p5=p + opts(title = "AR(1) Plot")                                   +
         geom_point( aes(residual,residualLag,group=group,col=group)) +
         stat_smooth(aes(residual,residualLag,group=group,col=group)) +
         xlab(expression(Residuals[t]))                               +
         ylab(expression(Residuals[t+1]))                             +
         geom_abline(intercept=0, slope=1)

  grid.newpage()
  if (length(order)>1)
     pushViewport(viewport(layout=grid.layout(floor(length(order)/2)+1,2))) else
     pushViewport(viewport(layout=grid.layout(1,1)))

   print(ifelse(!is.null(stuff), p1+stuff, p1), vp=vplayout(xCord(1),yCord(1)))
   print(ifelse(!is.null(stuff), p2+stuff, p2), vp=vplayout(xCord(1),yCord(1)))
   print(ifelse(!is.null(stuff), p3+stuff, p3), vp=vplayout(xCord(1),yCord(1)))
   print(ifelse(!is.null(stuff), p4+stuff, p4), vp=vplayout(xCord(1),yCord(1)))
   print(ifelse(!is.null(stuff), p5+stuff, p5), vp=vplayout(xCord(1),yCord(1)))
