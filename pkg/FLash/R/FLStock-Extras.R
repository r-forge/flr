expandAgeFLStock<-function(object,maxage,...)
    {
    if (class(object)!="FLStock") stop('not a FLStock object')
    if (!validObject(object)) stop('object not a valid FLStock')
    if (!(range(object,"max")== range(object,"plusgroup")) | (maxage<=range(object,"max"))) stop('maxage not valid')
    
    res      <-object
    dmns     <-dimnames(m(res))
    oldMaxage<-dims(res)$max
    dmns$age <-as.numeric(dmns$age[1]):maxage

    Pdiscard<-discards.n(res)[ac(oldMaxage)]/catch.n(res)[ac(oldMaxage)]
    Planding<-landings.n(res)[ac(oldMaxage)]/catch.n(res)[ac(oldMaxage)]
   
    slts<-c("catch.n",     
            "catch.wt",     
            "discards.n",  
            "discards.wt",     
            "landings.n",  
            "landings.wt",        
            "stock.n", 
            "stock.wt", 
            "m",          
            "mat",      
            "harvest", 
            "m.spwn", 
            "harvest.spwn")
                  
    ## create extra ages and fill with plusgroup                  
    for (i in slts) {
       dmns$iter  <-dimnames(slot(res,i))$iter
       slot(res,i)<-FLQuant(slot(res,i),dimnames=dmns)
       slot(res,i)[ac((oldMaxage+1):maxage)]<-0;
       slot(res,i)[ac((oldMaxage+1):maxage)]<-sweep(slot(res,i)[ac((oldMaxage+1):maxage)],2:6,slot(res,i)[ac(oldMaxage)],"+")
       }

    ## calc exp(-cum(Z)) i.e. the survivors 
    n               <-FLQuant(exp(-apply(slot(res,"m")[ac(oldMaxage:maxage)]@.Data,2:6,cumsum)-apply(slot(res,"harvest")[ac(oldMaxage:maxage)]@.Data,2:6,cumsum)))
    n[ac(maxage)]<-n[ac(maxage)]*(-1.0/(exp(-harvest(res)[ac(maxage)]-m(res)[ac(maxage)])-1.0))
    n               <-sweep(n,2:6,apply(n,2:6,sum),"/")
    stock.n(res)[ac((oldMaxage):maxage)]<-sweep(stock.n(res)[ac((oldMaxage):maxage)],1:6,n,"*")

    z<-harvest(res)[ac(maxage)]+m(res)[ac(maxage)]
    
    catch.n(res)[   ac((oldMaxage):maxage)]<-sweep(stock.n(res)[ac((oldMaxage):maxage)],2:6,harvest(res)[ac(maxage)]/z*(1-exp(-z)),"*")    
    discards.n(res)[ac((oldMaxage):maxage)]<-sweep(catch.n(res)[ac((oldMaxage):maxage)],2:6,Pdiscard,"*")
    landings.n(res)[ac((oldMaxage):maxage)]<-sweep(catch.n(res)[ac((oldMaxage):maxage)],2:6,Planding,"*")
    
    range(res,"max")      <-maxage
    range(res,"plusgroup")<-maxage

    ## replace any slots passed in (...)
    args <-names(list(...))
    slots<-getSlots("FLBRP")
    for (i in args)
	    if (any(i==names(slots)))
        if (class(list(...)[[i]])==slots[i]) slot(res,i)<-list(...)[[i]]

     if (!validObject(res)) stop("Not valid")

    return(res)
    }

setMethod("merge", signature(x="FLStock", y="FLStock"),
   function(x, y, ...)
    {
    if (!all(unlist(dims(ple4))==unlist(dims(ple4)))) stop("FLStock objects to combine have dim mismatch")

    res<-FLStock(stock     =stock(     x)   +stock(  y),
                 stock.n   =stock.n(   x)   +stock.n(y),
                 catch     =catch(     x)   +catch(  y),
                 catch.n   =catch.n(   x)   +catch.n(y),
                 landings  =landings(  x)+landings(  y),
                 landings.n=landings.n(x)+landings.n(y),
                 discards  =discards(  x)+discards(  y),
                 discards.n=discards.n(x)+discards.n(y))

    name(res) = paste(name(x),"merged with", name(y))
    desc(res) = paste(desc(x),"merged with", desc(y))
    res@range = x@range

    stock.wt(res)   <-(   stock.wt(x)*stock.n(x)      +stock.wt(y)*stock.n(y))/(stock.n(x)+stock.n(y))
    catch.wt(res)   <-(   catch.wt(x)*catch.n(x)      +catch.wt(y)*catch.n(y))/(catch.n(x)+catch.n(y))
    landings.wt(res)<-(landings.wt(x)*landings.n(x)+landings.wt(y)*landings.n(y))/(landings.n(x)+landings.n(y))
    discards.wt(res)<-(discards.wt(x)*discards.n(x)+discards.wt(y)*discards.n(y))/(discards.n(x)+discards.n(y))

    args <- list(...)

    if (!is.null(args) && any(names(args) == "m"))
       m(res)<-args$m
    else
       {
       warning("adding m slots, this might not be want you want")
       m(res)<-(m(x)+m(y))/2
       }
       
    if (!is.null(args) && any(names(args) == "m.spwn"))
       m.spwn(res)<-args$m.spwn
    else
       {
       warning("adding m.spwn slots, this might not be want you want")
       m.spwn(res)      <-(harvest.spwn(x)+harvest.spwn(y))/2
       }
       
    if (!is.null(args) && any(names(args) == "harvest.spwn"))
       harvest.spwn(res)<-args$harvest.spwn
    else
       {
       warning("adding harvest.spwn slots, this might not be want you want")
       harvest.spwn(res)<-(harvest.spwn(x)+harvest.spwn(y))/2
       }
        
    mat(res)    <-(mat(x)*stock.n(x)+mat(y)*stock.n(y))/(stock.n(x)+stock.n(y))
    harvest(res)<-calcF(m(res),catch.n(res),stock.n(res))

    return(res)
    }
)   # }}}

# "+"      {{{
setMethod("+", signature(e1="FLStock", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)