
setGeneric("diagnostics", function(object, ...){
	standardGeneric("diagnostics")
	}
)

setMethod("diagnostics", signature(object="FLICA"),

  function(object, type=c("normal.index","all.catch"),
              par.settings=NULL,  show.index=NULL,show.age=NULL,main.title=TRUE,
              plot.titles =TRUE,show.grid=TRUE,...){

  #Need to allow the two different types of diagnostics to come closer together
  #Add some functionality to allow better customisation of plots

  #Set default plot parameters and then override as necessary
  #----------------------------------------------------------
  
  par.settings.input   <- par.settings  
  par.settings <- list(mfrow=c(3,2),las=0,oma=c(0,0,3,0),mgp=c(1.75,0.5,0),
                          mar=c(3,3,2.5,1),cex.main=1,tck=-0.01)
  for(i in names(par.settings.input)) {
    par.settings[[i]] <- par.settings.input[[i]]    #Overwrite defaults
  }

  #Start by extracting all useful data from ica.out to be plotted later
  #--------------------------------------------------------------------
  #Specifically, we are interested in the values of index@age, FLICA@stock.n and index.res@age

  #Flag whether par settings and type are specified
  par.settings.specified <- !missing(par.settings)
  type.missing           <- missing(type)

  ica.out <- object
  #Check that the index names are setup correctly - if not, just use numbers
  if(any(duplicated(names(ica.out@index)))) {
    warning("Duplicate and/or missing names found in index slot. Surveys have been allocated \nnames instead. Try setting the @names slot in the FLIndices object you feed to FLICA.\n")
    names(ica.out@index)      <- paste("Survey",sequence(length(names(ica.out@index))))
    names(ica.out@index.res)  <- paste("Survey",sequence(length(names(ica.out@index.res))))
  }

  #Extract survey names and types
  surv.names  <-  names(ica.out@index)
  surv.types  <-  lapply(ica.out@index,attr,"type")

  #Convert FLQuants to data.frames
  index.df      <-  as.data.frame(ica.out@index)
  index.res.df  <-  as.data.frame(ica.out@index.res)
  index.hat.df  <-  as.data.frame(ica.out@index.hat)
  names(index.hat.df)  <- gsub("data","index.hat",names(index.hat.df))
  names(index.res.df)  <- gsub("data","index.res",names(index.res.df))
  names(index.df)  <- gsub("data","index",names(index.df))

  #Check that the dimensions of index and index.res line up
  if(nrow(index.df)!=nrow(index.res.df) | nrow(index.df)!=nrow(index.hat.df)) {
    stop("Data in index, indes.res and/or index.hat slots are of different sizes.")
  }
  #Now merge everything together into a single data frame
  data.set  <-  merge(index.df,index.res.df)
  data.set  <-  merge(data.set,index.hat.df)
  names(data.set)  <- gsub("qname","survey",names(data.set))

  #Filter for a specific index and age, if requested
  if(!is.null(show.age))  data.set <- subset(data.set,age%in%show.age)
  if(!is.null(show.index))data.set <- subset(data.set,survey%in%show.index)

  #And finally split it for further processing
  data.l    <-  split(data.set,list(data.set$age,data.set$survey),drop=TRUE)

  #Now plot the index diagnostics
  #------------------------------
  lapply(data.l,function(dat) {
      #Filter missing values by setting to NA -99 is hard coded into FLICA
      rows.to.omit    <-  !is.finite(dat$index) | !is.finite(dat$index.hat) | !is.finite(dat$index.res)
      rows.to.omit    <-  rows.to.omit | dat$index@.Data == -99 |dat$index.res@.Data == -99 |dat$index.hat@.Data == -99
      dat[rows.to.omit,c("index","index.res","index.hat")] <- NA

      #Omit leading and trailing NAs
      year.rng  <-  range(dat$year[!is.na(dat$index)])
      dat       <- subset(dat,year%in%seq(year.rng[1],year.rng[2]))

      #Get and set information about survey
      surv.age    <-  unique(dat$age)
      surv.name   <-  unique(dat$survey)
      surv.num    <-  which(surv.names %in% surv.name)
      surv.typ    <-  surv.types[[surv.num]]
      stock.label <-  switch(surv.typ,
                              number=paste("Estimated Stock Numbers, age",surv.age),
                              biomass=paste("Estimated Biomass"),
                              "Unknown survey type")
      index.mdl   <-  ica.out@control@index.model[surv.num]
      ttl         <-  switch(surv.typ,
                              number=paste(surv.name,", age ",surv.age,", diagnostics",sep=""),
                              biomass=paste(surv.name,", diagnostics",sep=""))

      #If insufficient information (eg vertical range), don't plot it
      #Sometimes FLICA returns an "empty" index slot for unknown reasons.
      if(all(dat$index==0)|all(dat$index==1)) {
        warning(paste("Insufficient information in ",surv.name," index slot (age ",surv.age,") to plot diagnostics.",sep=""))
        return(NULL)
      }

      #Get catchability model parameters
      model.str  <-  switch(surv.typ,
                              number=paste("Index ",surv.num,", age ",surv.age," numbers, Q",sep=""),
                              biomass=paste("Index ", surv.num,", biomass, Q",sep=""))
      Q           <-  switch(index.mdl,
                              absolute=1,
                              linear=ica.out@param[model.str,"Value"],
                              power=ica.out@param[model.str,"Value"],
                              stop("Unrecognised survey catchability model in \"@control@index.model\" slot."))
      K           <-  switch(index.mdl,
                              absolute=1,
                              linear=1,
                              power=ica.out@param[gsub("Q","K",model.str),"Value"],
                              stop("Unrecognised survey catchability model in \"@control@index.model\" slot."))

      #Construct variance, covariance matrix and use it to estimate the variance in index.hat
      vcov.matrix <-  switch(index.mdl,
            absolute=matrix(0,nrow=2,ncol=2),   #No errors there
            power={ model.strs <- c(gsub("Q","K",model.str),model.str)
                    ica.out@covar[model.strs,model.strs]},
            linear={opt.matrix <- matrix(0,nrow=2,ncol=2)
                    opt.matrix[2,2] <-  ica.out@covar[model.str,model.str]    #Variance of catchability
                    opt.matrix},
            stop("Unrecognised survey catchability model in in \"@control@index.model\" slot."))
      g   <-  as.matrix(c(K,1))     #Vector of gradients for the model logI=logQ + K logN ie [dlogI/dlogN,
      var.index.hat <-  t(g) %*% vcov.matrix %*% g    #Variance in an index estimated from given stoc size, using the delta method

      #Estimate stock numbers/biomass by back-calculating from the index.hat value, and then estimate the
      #error in index.hat by forward calculation
      dat$stock <-  (dat$index.hat/Q)^(1/K)
      dat$index.hat.ul  <-  exp(log(dat$index.hat)+1.65*sqrt(var.index.hat))
      dat$index.hat.ll  <-  exp(log(dat$index.hat)-1.65*sqrt(var.index.hat))

      #Find a common stock axis scalar
      stock.rng     <-  range(dat$stock,na.rm=TRUE)
      stock.lim     <-  range(c(0,stock.rng))
      stock.exp     <-  floor(log10(max(pretty(stock.lim)))/3)*3
      stock.div     <-  10^stock.exp
      if(units(ica.out@stock.n)!="NA") {stock.label <- paste(stock.label," (",units(ica.out@stock.n),")",sep="")}
      if(stock.exp!=0) {
        stock.label   <-  bquote(paste(.(stock.label)," [",10^.(stock.exp),"]"))
        }
      dat$stock     <-  dat$stock/stock.div
      stock.lim     <-  stock.lim/stock.div
      stock.rng     <-  stock.rng/stock.div

      #Scale index axis
      idx.rng       <-  range(c(dat$index,dat$index.hat),na.rm=TRUE)
      idx.lim       <-  range(c(0,idx.rng))
      idx.exp       <-  floor(log10(max(pretty(idx.lim)))/3)*3
      idx.div       <-  10^idx.exp
      idx.label     <-  "Index "
      if(units(ica.out@index[[surv.num]])!="NA") {idx.label <- paste(idx.label," (",units(ica.out@index[[surv.num]]),")",sep="")}
      if(idx.exp!=0) {idx.label     <-  bquote(expression(paste(.(idx.label)," [",10^.(idx.exp),"]")))}
      dat$index     <-  dat$index/idx.div
      dat$index.hat <-  dat$index.hat/idx.div
      dat$index.hat.ul  <- dat$index.hat.ul/idx.div
      dat$index.hat.ll  <- dat$index.hat.ll/idx.div
      idx.rng       <-  idx.rng/idx.div
      idx.lim       <-  idx.lim/idx.div

      #Setup plots
      oldpar <-  do.call(par,par.settings)
      plot.counter  <- 0

      #Plot 1: Index comparison
      if(any(c("index","all","all.index","normal.index") %in% type)) {
        plot(index.hat ~ year, dat,ylim=idx.lim,ann=FALSE,axes=FALSE,type="n",...)
        leg.args  <-  list("topleft",legend=c("Observed","Fitted"),
                          lty=c(NA,1),pch=c(19,4),horiz=TRUE,bg=par("bg"))
        leg.lims <-  do.call(legend,c(leg.args,plot=FALSE))
        par(new=TRUE)
        plot(index.hat ~ year, dat,xlab="Year",ylab=idx.label,
              type="n",new=FALSE,xaxt="n",
              ylim=c(0,par("usr")[4]+leg.lims$rect$h))
        axis(1,at=unique(round(axTicks(1),0)))
        if(show.grid) grid()
        lines(index.hat ~year, na.omit(dat),lty=1)
        points(index.hat ~year, dat,pch=4)
#        lines(index.hat.ul ~year, dat,lty=2)
#        lines(index.hat.ll ~year, dat,lty=2)
        points(index ~ year,dat,pch=19,type="p")
        do.call(legend,leg.args)
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main="a) Observed and fitted index time series")
        }
      }

      #Plot 2: Catchability
      if(any(c("catchability","all","all.index","normal.index") %in% type)) {
        plot(index ~stock, dat,ann=FALSE,axes=FALSE,type="n",...)
        leg.args <-  list("topleft",legend=c(paste("Fitted", index.mdl,"model"),"95% CI of fit"),lty=c(1,2),pch=c(NA,NA),horiz=TRUE,bg=par("bg"))
        leg.lims <-  do.call(legend,c(leg.args,plot=FALSE))
        par(new=TRUE)
        plot(index ~stock, dat,xlab=stock.label,ylab=idx.label,type="n",new=FALSE,
            ylim=c(par("usr")[3],par("usr")[4]+leg.lims$rec$h))
        if(show.grid) grid()
        points(index ~stock, dat,pch=19)
        horiz.seq <- seq(par("usr")[1],par("usr")[2],length.out=50)
        catchability.fit  <-  Q*(horiz.seq*stock.div)^K/idx.div
        catchability.fit.ul <-  exp(log(catchability.fit)+1.96*sqrt(var.index.hat))
        catchability.fit.ll <-  exp(log(catchability.fit)-1.96*sqrt(var.index.hat))
        lines(catchability.fit ~ horiz.seq)
        lines(catchability.fit.ul ~ horiz.seq,lty=2)
        lines(catchability.fit.ll ~ horiz.seq,lty=2)
        do.call(legend,leg.args)
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main=paste(letters[plot.counter],") Catchability",sep=""))
        }
      }

      #Plot 3: Residuals vs time
      if(any(c("time","all","all.index","normal.index") %in% type)) {
        plot(index.res ~year, dat, ylab="Log Residuals", xlab="Year",type="n",xaxt="n",...)
        axis(1,at=unique(round(axTicks(1),0)))
        if(show.grid) grid()
        points(index.res ~year, dat, type="h")
        points(index.res ~year, dat, pch=19,cex=0.75)
        abline(h=0)
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main=paste(letters[plot.counter],") Index residuals over time",sep=""))
        }
      }

      #Plot 4: Regression Residuals
      if(any(c("stock","all","all.index","normal.index") %in% type)) {
        plot(index.res ~ stock,dat,xlab=stock.label,ylab="Log Residuals",type="n",...)
        if(show.grid) grid()
        points(index.res ~ stock,dat,pch=19)
        abline(h=0)
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main=paste(letters[plot.counter],") Index residuals vs stock",sep=""))
        }
      }

      #Plot 5: Normal Q-Q plot
      if(any(c("qq","all","all.index","normal.index") %in% type)) {
        qq.data <-  qqnorm(dat$index.res,xlab="Normal Quantiles",ylab="Log Residuals",pch=19,main="",
                        panel.first=grid(lty=ifelse(show.grid,"dotted",0)),...)
        qq.fit  <-  lm(qq.data$y~qq.data$x)
        #Make prediction, but supress the warning it throws
        warn  <-  getOption("warn")
        options(warn=-1)
				qq.ci   <-  data.frame(x=na.omit(qq.data$x),predict(qq.fit,interval="prediction",level=0.90))
				options(warn=warn)
				qq.ci   <-  qq.ci[order(qq.ci$x),]
        lines(qq.ci$x,qq.ci$fit,lty=1)
        lines(qq.ci$x,qq.ci$upr,lty=2)
        lines(qq.ci$x,qq.ci$lwr,lty=2)
        legend("topleft",legend=c("Fit","90% Conf. Int."),lty=c(1,2),pch=c(NA,NA),bg=par("bg"))
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main=paste(letters[plot.counter],") Normal Q-Q plot",sep=""))
        }
      }

      #Plot 6: Autocorrelation
      if(any(c("acf","all","all.index") %in% type)) {
        acf(na.contiguous(as.ts(dat$index.res)),ylab="ACF",xlab="Lag (yrs)",type=c("partial"),
            panel.first=grid(lty=ifelse(show.grid,"dotted",0)),ci.col="black",main="",...)
        legend("topright",legend=c("95% Conf. Int."),lty=c(2),pch=c(NA),horiz=TRUE,box.lty=0)
        if(plot.titles) {
          plot.counter  <- plot.counter + 1
          title(main=paste(letters[plot.counter],") Autocorrelation of Residuals",sep=""))
        }
      }

      #Title at the top
      if(main.title) {title(main=ttl,outer=TRUE)}

      #Restore plot settings
      par(oldpar)

    }) #End lapply


  # Now prepare the catch diagnostics
  # ---------------------------------
  #Reset the plot counter
  catch.plot.counter  <- 0

  #Weight catch residuals appropriately
  age.wt        <-  object@control@lambda.age
  yr.wt         <-  object@control@lambda.yr
  catch.res.wt   <-  sweep(object@catch.res,1,age.wt,"*")    #First weight by ages
  catch.res.wt   <-  sweep(catch.res.wt,2,yr.wt,"*")          #Then by years
  
  #Filter the rubbish
  catch.res.wt[!is.finite(catch.res.wt)]   <-  NA
  catch.res.df   <-  as.data.frame(catch.res.wt)
  
  #Setup figure
  oldpar <- do.call(par,par.settings)
  
  # Catch diagnostic plots
  # ---------------------------
  # Catch residuals bubble plot
  if(any(c("bubbles","all","all.catch") %in% type)) {
    #Filter out missing data points by setting equal to NA
    filter <- NULL
    bub.scale  <-  5
    catch.res.df$data   <- ifelse(catch.res.df$data %in% filter,NA, catch.res.df$data)
    catch.res.df$abs    <-  abs(catch.res.df$data)
    catch.res.df        <-  catch.res.df[order(catch.res.df$abs,decreasing=TRUE),]
    cex0          <- abs(catch.res.df$data)
    cexn          <- bub.scale * cex0/max(cex0, na.rm = TRUE) + bub.scale * 0.1
    negs          <-  (catch.res.df$data<0)
    #Setup plot
    plot(age~year,catch.res.df,type="n",xlab="Year",ylab="Age",...)
    #Diagonal lines
    if(show.grid) {
      a.min     <-  max(catch.res.df$age)-min(catch.res.df$year)
      a.max     <-  min(catch.res.df$age)-max(catch.res.df$year)
      lapply(as.list(seq(a.min,a.max)),function(a) {
          abline(a=a,b=1,lty=3,col="lightgrey")})
    }
    #plot Circles
    points(age~year,catch.res.df[,],pch=21,bg="white",cex=cexn)
    points(age~year,catch.res.df[negs,],pch=21,bg="grey",cex=cexn[negs])
    if(plot.titles) {
      catch.plot.counter  <- catch.plot.counter + 1
      title(main=paste(letters[catch.plot.counter],") Catch Residuals",sep=""))
    }
    max.row <-  catch.res.df[which.max(catch.res.df$abs),]
    text(max.row$year,max.row$age,label=sprintf("%.2f",round(max.row$data,2)),cex=0.8)
  }
  
  # Selectivity
  if(any(c("selectivity","all","all.catch") %in% type)) {
    sel     <-  as.data.frame(yearMeans(object@sel))[c("age","data")]
    #Get conidence limits in selectivity
    sel.CLs  <-  subset(object@param,Param=="Sel",select=c("Age","Lower.95.pct.CL","Upper.95.pct.CL"))
    sel.CLs$Age   <-  as.numeric(sel.CLs$Age)
    plot(data ~ age, sel, type="n",ylim=c(0,max(sel$data, sel.CLs$Upper.95.pct.CL, na.rm=T)),
          ylab="Selectivity",xlab="Age",...)
    if(show.grid) grid()
    lines(data ~ age, sel,type="l",lwd=3)
    segments(sel.CLs$Age,sel.CLs$Lower.95.pct.CL,sel.CLs$Age,sel.CLs$Upper.95.pct.CL)
    if(plot.titles) {
      catch.plot.counter  <- catch.plot.counter + 1
      title(main=paste(letters[catch.plot.counter],") Selection Pattern",sep=""))
    }
  }
  
  # Marginal totals: by Year
  if(any(c("mt.year","all","all.catch") %in% type)) {
    marginal.yr <- quantSums(catch.res.wt)
    xlabel <- dimnames(marginal.yr)$year
    barplot(as.vector(marginal.yr),names.arg=xlabel,space=0,
        ylab="Marginal Total",xlab="Year",...)
    if(plot.titles) {
      catch.plot.counter  <- catch.plot.counter + 1
      title(main=paste(letters[catch.plot.counter],") Year Residuals",sep=""))
    }
  }
  
  # Marginal totals: by Age
  if(any(c("mt.age","all","all.catch") %in% type)) {
    marginal.a <- yearSums(catch.res.wt)
    xttl <- dimnames(marginal.a)$age
    barplot(as.vector(marginal.a),names.arg=xttl,space=0,
        ylab="Marginal Total",xlab="Age",...)
    if(plot.titles) {
      catch.plot.counter  <- catch.plot.counter + 1
      title(main=paste(letters[catch.plot.counter],") Age Residuals",sep=""))
    }
  }

  #Tidy up, finish off, but only if we actually plotted something
  if(catch.plot.counter!=0) {
    if(main.title) {title(main="Fitted catch diagnostics",outer=TRUE)}
    par(oldpar)
  }      
  return(invisible(NULL))

})
