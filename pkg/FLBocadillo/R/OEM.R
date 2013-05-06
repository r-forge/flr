## Observation Error Model

## fwd(FLStock)
if (!isGeneric("OEM"))
  setGeneric("OEM", function(object, ...)
	  standardGeneric("OEM"))

setMethod("OEM", signature(object="FLStock"),
OEM<-function(object,start,type="stock",end=NULL,plusgroup=NULL,...)
    {
    if (missing(end)) end<-start
    yrs<-as.character(end:start)
    stk<-window(object, start=start, end=end)

    ## replace any slots
    args <- list(...)
    slt<-names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
    for(i in names(args)[names(args) %in% names(slt)[slt=="FLQuant"]]){
       yrs.      <-yrs[yrs %in% dimnames(slot(res, i))$year]
       slot(res, i)[,yrs.]<-i[,yrs.]}

    if (!is.null(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)

    return(stk)
    })

## CPUE Index
OEMIndex<-function(stk,start,end=NULL,plusgroup=NULL,startf=NULL,endf=NULL,deviates=NULL)
     {
     ## unbiased population estimates
     if (missing(end)) end<-start
     yrs<-start:end

     stk<-window(stk,start=start,end=end)
     if (!is.null(plusgroup))
        stk<-setPlusGroup(stk,plusgroup)@n

     idx<-as(stk,"FLIndex")

     if (!is.null(startf)) idx@range["startf"]<-startf
     if (!is.null(endf))   idx@range["endf"]  <-endf

     if (!is.null(deviates))
        idx@index<-idx@index*deviates[dimnames(idx@index)$age,ac(yrs)]

     return(idx)
     }

## function to apply a linearly increasing trend to an FLQuant
biasLinear<-function(x,obj)
   {
   if (x>0)
      res  <-1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x)
   else
      res  <-sort(1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x),d=T)

   return(obj*FLQuant(rep(res,each=dims(obj)$age),dimnames=dimnames(obj)))
   }

