admbIn<-function(file){
  ## read in data from ADMB Par or Rep file
  dat  <-scan(file,what="",sep="\n",skip=1)

  ## Get values
  vals <-lapply(strsplit(dat[grep("#",dat,invert=TRUE)]," "), function(x) as.numeric(x[nchar(x)>0]))

  ## name elements
  names(vals)<-lapply(grep("#",dat,value=T),function(x) substr(x,3,nchar(x)-1))

  return(vals)}

admbOut<-function(x,file){
  cat(x[[1]],file=file,append=FALSE)
  cat("\n",file=file,append=TRUE)
  for (i in 2:length(x)){
    cat(x[[i]],file=file,append=TRUE)
    cat("\n",file=file,append=TRUE)}}
