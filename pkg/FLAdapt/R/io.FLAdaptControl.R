FLAdaptControl <- function(file=""){
	
	set.param<-function(vals){
		param<-data.frame(cbind(lower =vals[,1],
								best  =vals[,2],
								upper =vals[,3],
								method=vals[,4],
								sigma =vals[,5],
								n     =1))

		param
		}

	res <- .Call("readFLAdaptControl", file);
	
	res@param.termage<-set.param(res@param.termage)
	res@param.fratio <-set.param(res@param.fratio)
	res@param.srr    <-set.param(res@param.srr)
	res@param.var    <-set.param(res@param.var)
	res@param.q      <-set.param(res@param.q)

		
	return(res)}
