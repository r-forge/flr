dirMy="/home/lkell/flr/manual/ggplotFL/inputs/bft-e"
tmp=apply(expand.grid(Implementation=c("Error","Perfect"),
                      Catch         =c("Inflated","Reported"),
                      Run           =c(13,15),
                      Recruitment   =c("High","Low","Medium"),
                      Steepness     =c("0.5","0.75","0.9")),2,ac)

filePrj=apply(tmp,1,function(x) paste(dirMy,paste(x,collapse="/"),sep="/"))

bftRef<-readPro2Box(paste(filePrj[1],"BENCH-1.OUT",sep="/"),type="ref")
bftSta<-readPro2Box(filePrj[1],type="sta")
bftOut<-readPro2Box(filePrj[1],type="out")
