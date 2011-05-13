## Updates wt in FLStock plus group when projecting by modelling mean age
fwdPgMeanAge=function(x,F,sr,vB,quantity="catch",pgAge=10,yrs=19:0){
  for (i in yrs){
      x  =fwd(x,ctrl=fwdControl(data.frame(quantity=quantity,val=F,year=dims(x)$maxyear-i)),sr=sr)
      stock.wt(x)[ac(pgAge),ac(dims(x)$maxyear-i)] =vonBMass(pgAge(x)[,ac(dims(x)$maxyear-i)],FLPar(vB))
      }

  wt(x)    =stock.wt(x)
  catch(x)=computeCatch(x,"all")

  return(x)}
