setMethod('[[', signature(x='FLComp', i='character'),
	function(x, i, ..., drop=FALSE) {

    res <- FLQuants()

    for (j in 1:length(i))
      res[[i[j]]] <- do.call(i[j],list(x))

    names(res) <- i

    return(res)})
