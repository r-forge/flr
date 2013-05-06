dimFLComp<-function(x)
	{
	dms <- unlist(dims(obj)[c("age","year","unit","season","area","iter")])
    return(dms)
    }
