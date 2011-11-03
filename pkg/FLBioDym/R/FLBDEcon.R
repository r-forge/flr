validFLBDEcon <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)} 

setClass('FLBDEcon', representation(
    "FLBioDym",
    effort        ='FLQuant',
    q             ='FLQuant',
    fcost         ='FLQuant',
    vcost         ='FLQuant'),
  prototype(
    catch       =FLQuant(),
    index       =FLQuant(),
    fitted      =FLQuant(),
    stock       =FLQuant()
    ),
  validity=validFLBDEcon) 
