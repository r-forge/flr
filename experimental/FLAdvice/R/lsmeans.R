
#### Get LSMeans for delta-
lsmeansDelta<-function(x,xBin,newdat=NULLsetMethod('ln2z', signature(object='numeric'),
    function(object,a=0.001,b=3,wt="stock.wt") 
	  ln2zFunc(object,a,b,wt))){

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

#### Get LSMeans 
lsmeans<-function(x,newdat=NULL){

  if (is.null(newdat))
    newdat<.expand.grid()

  prd       <-data.frame(newdat, predict(x,    newdat,se.fit = TRUE, type="response"))
  prd$low   <-prd$index*(1-2*prd$cv)
  prd$upper <-prd$index*(1+2*prd$cv)

  return(prd)}