#### Functions for input data from ASPIC #######################################
#### Time series
getBio<-function(fileBio){
   #### data and dims
   t.  <-scan(fileBio,skip=4)
   nits<-scan(fileBio,skip=1,nmax=1)
   yrs <-scan(fileBio,skip=2,nmax=2)
   yrs <-yrs[1]:yrs[2]
   nyrs<-diff(yrs)
   nval<-nyrs*2+3

   ncol <-length(scan(fileBio,skip=4,nlines=1))
   ncol2<-length(scan(fileBio,skip=5,nlines=1))

   b.<-FLQuant(t.[rep(1:ncol, nits)+rep((ncol+ncol2)*((1:nits)-1),     each=ncol )], dimnames=list(year=c(0,yrs[1:(ncol-1) ]), iter=1:nits))
   f.<-FLQuant(t.[rep(1:ncol2,nits)+rep((ncol+ncol2)*((1:nits)-1)+ncol,each=ncol2)], dimnames=list(year=c(0,yrs[1:(ncol2-1)]), iter=1:nits))

   bmsy<-b.[,1]
   fmsy<-f.[,1]

   b.<-b.[,-1]
   f.<-f.[,-1]

   return(list(stock=sweep(b.,6,bmsy,"/"),harvest=sweep(f.,6,fmsy,"/"),bmsy=bmsy,fmsy=fmsy))
   }

readFLStock <- function (file, file2="NULL",type = "VPA", name, desc = paste("Imported from a",
    type, "file. (", file, "). ", date()), m = 0.2, quant="age", quiet=TRUE, no.discards=FALSE,timeOfFish=0.5)
 	  {
	  if (type!="mfcl")
       return(FLCore::readFLStock(file,type,name,desc,m,quant,quiet,no.discards)) else
       return(mfclFLStock(fileRep=file,filePar=file2,timeOfFish))
    }

