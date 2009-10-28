## "[<-"            {{{
setMethod("[<-", signature(x="FLIndex"),
	function(x, i, j, k, l, m, n, ..., value="missing") {

		qnames <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		dx <- unlist(dims(x)[c("age","year","unit","season","area","iter")])

		if (missing(i))
			i <- seq(1, dx[1])
		if (missing(j))
			j <- seq(1, dx[2])
   		if (missing(k))
			k <- seq(1, dx[3])
		if (missing(l))
			l <- seq(1, dx[4])
		if (missing(m))
			m <- seq(1, dx[5])
		if (missing(n))
			n <- seq(1, dx[6])

    for(q in qnames){
       if (dim(slot(value, q))[6]==1) n.<-1 else n.<-n
       slot(x, q)[,j,k,l,m,n.] <- slot(value, q)}

   		return(x)
	}
)   # }}}
