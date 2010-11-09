## gets dims in FLPar that match Dims in FLQuant/FLCohort
setGeneric('orderFLPar', function(e1, e2, ...)
  standardGeneric('orderFLPar'))

orderPar<-function(flp,flq){

    if (!(validObject(flp) & validObject(flq)))
       stop("Input objects are not valid: validObject == FALSE")

    #### Check parameters and FLQuant dims

    #### get parameter names
    dimParams        <-2:(length(dim(flp)))
    nmsParams        <-names(dimnames(flp))
    names(dimParams) <-nmsParams[!(nmsParams %in% "params")]

    #### get oject names
    dimFLQuant       <-1:6
    nmsObject        <-names(dimnames(flq))
    names(dimFLQuant)<-nmsObject

    #### Check that parameters are in FLQuant
    if (!all(nmsParams %in% nmsObject))
       ("Stop extra dims in ´FLPar´")

    #### check  FLPar elements in same order as FLQuant
    if (any(dimParams!=sort(dimParams))) stop("Wrong order of ´FLPar´ dims")
    #This will correct the order params<-aperm(params,c(1,dimParams))

    return(dimFLQuant[names(dimParams)])}

setMethod("orderFLPar", signature(e1="FLPar",   e2="FLQuant" ), function(e1,e2) orderPar(e1,e2))
setMethod("orderFLPar", signature(e1="FLQuant", e2="FLPar"   ), function(e1,e2) orderPar(e2,e1))
setMethod("orderFLPar", signature(e1="FLPar",   e2="FLCohort"), function(e1,e2) orderPar(e1,e2))
setMethod("orderFLPar", signature(e1="FLCohort",e2="FLPar"),    function(e1,e2) orderPar(e2,e1))

#### FLQuant ###################################################################
setMethod("+", signature(e1="FLPar",  e2="FLQuant"), function(e1, e2) FLQuant(sweep(e2, orderFLPar(e1,e2), e1, "+")))
setMethod("+", signature(e1="FLQuant",e2="FLPar"),   function(e1, e2) FLQuant( e2+e1))

setMethod("-", signature(e1="FLPar",  e2="FLQuant"), function(e1, e2) FLQuant(sweep(e2, orderFLPar(e1,e2), e1, "-")))
setMethod("-", signature(e1="FLQuant",e2="FLPar"),   function(e1, e2) FLQuant(-e2+e1))

setMethod("*", signature(e1="FLPar",  e2="FLQuant"), function(e1, e2) FLQuant(sweep(e2, orderFLPar(e1,e2), e1, "*")))
setMethod("*", signature(e1="FLQuant",e2="FLPar"),   function(e1, e2) FLQuant( e2*e1))

setMethod("/", signature(e1="FLPar",  e2="FLQuant"), function(e1, e2) FLQuant(sweep(e2, orderFLPar(e1,e2), e1, "/")))
setMethod("/", signature(e1="FLQuant",e2="FLPar"),   function(e1, e2) FLQuant(e2*(1/e1)))

setMethod("^", signature(e1="FLQuant",e2="FLPar"),   function(e1, e2) FLQuant(sweep(e2, orderFLPar(e1,e2), e1, "^")))
setMethod("^", signature(e1="FLPar",  e2="FLQuant"), function(e1, e2) FLQuant(exp(e2*log(e1))))

#### FLCohort ##################################################################
setMethod("+", signature(e1="FLPar",   e2="FLCohort"), function(e1, e2)  FLCohort(sweep(e2, orderFLPar(e1,e2), e1, "+")))
setMethod("+", signature(e1="FLCohort",e2="FLPar"),    function(e1, e2)  FLCohort(e2+e1))

setMethod("-", signature(e1="FLPar",   e2="FLCohort"), function(e1, e2)  FLCohort(sweep(e2, orderFLPar(e1,e2), e1, "-")))
setMethod("-", signature(e1="FLCohort",e2="FLPar"),    function(e1, e2)  FLCohort(-e2+e1))

setMethod("*", signature(e1="FLPar",   e2="FLCohort"), function(e1, e2)  FLCohort(sweep(e2, orderFLPar(e1,e2), e1, "*")))
setMethod("*", signature(e1="FLCohort",e2="FLPar"),    function(e1, e2)  FLCohort(e2*e1))

setMethod("/", signature(e1="FLPar",   e2="FLCohort"), function(e1, e2)  FLCohort(sweep(e2, orderFLPar(e1,e2), e1, "/")))
setMethod("/", signature(e1="FLCohort",e2="FLPar"),    function(e1, e2)  FLCohort(e2*(1/e1)))

setMethod("^", signature(e1="FLPar",   e2="FLCohort"), function(e1, e2)  FLCohort(sweep(e2, orderFLPar(e1,e2), e1, "^")))
setMethod("^", signature(e1="FLCohort",e2="FLPar"),    function(e1, e2)  FLCohort(exp(e2*log(e1))))
