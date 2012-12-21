## moments

moments<-function(x,n,p=1) (sum(x^p*n)/sum(n))^(1/p)
decade <-function(x) x-(x %% 10)

stdz  <-function(x,na.rm=TRUE) ((x-mean(x,na.rm=na.rm))/sd(x,na.rm=na.rm))
minMax<-function(x,na.rm=TRUE) (x-min(x,na.rm=na.rm))/diff(range(x,na.rm=na.rm))


cols=function(x) {
  neg=colorRamp(c("white","blue"))
  pos=colorRamp(c("white","red"))
  
  res=mdply(as.vector(x), function(x) if (x>0) pos(x) else neg(abs(x)))[,-1]
  names(res)=c("r","g","b")
  
  res=mdply(res,rgb2hsv)[,4:6]
  names(res)=c("h","s","v") 
  mdply(res,hsv)[,4]}


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
