## get cpue
aspicCpues=function(object){
     if (!file.exists(object)) stop(paste("file", object, "does not exist"))
     inp=scan(object,sep="\n",what=character())
 
     getU=function(cpue,inp){
            tmp2=inp[cpue]
            tmp3=unlist(strsplit(tmp2," "))
            tmp4=as.numeric(tmp3[nchar(tmp3)>0])
            tmp5=as.data.frame(t(array(tmp4,dim=c(3,length(tmp4)/3))))
            names(tmp5)=c("year","cpue","catch")
          
            catch=tmp5[,c(1,3)]
            names(catch)[2]="data"
            
            cpue =tmp5[,1:2]
            names(cpue)[2]="data"
            cpue$data[cpue$data<=0]=NA
                     
            res=cpue(index=as.FLQuant(cpue),catch=as.FLQuant(catch))
          
            return(res)}
      
  CC =grep("CC",substr(inp,1,2))
  nms=inp[CC-1]
        
  u =mlply(data.frame(from=CC+1,to=c(CC[-1]-2,length(inp))),seq)
        
  u=cpues(llply(u,getU, inp=inp))
  names(u)=nms
      
  return(u)}
