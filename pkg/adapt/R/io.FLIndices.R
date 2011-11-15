
## readVPA2BoxIndices		{{{
readVPA2BoxIndices <- function(file.,na.strings="NA") {
    skip.hash<-function(i) {
        i<-i+1
        while (substr(scan(file.,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
            i<-i+1
        return(i)
    }
    skip.until.minus.1<-function(i) {
        i<-i+1
        while (scan(file.,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
            i<-i+1
        return(i)
    }

    # range
    range<-numeric(5)
	names(range)<-c("min","max","plusgroup","minyear","maxyear")
	i <- skip.hash(0)
    range[c("minyear", "maxyear")] <- scan(file.,skip = i, nlines = 1, nmax = 2, quiet = TRUE,na.strings=na.strings)
    i <- skip.hash(i)
    range[c("min", "max", "plusgroup")] <- scan(file.,skip = i, nlines = 1, nmax = 3, quiet = TRUE,na.strings=na.strings)
    i <- skip.hash(i+1)
    NIndex <- scan(file.,skip=i,nlines=1,nmax=1,quiet=TRUE)
    i <- skip.until.minus.1(i)
    i <- skip.hash(i)
    smry. <- array(0,dim=c(NIndex,7))
      
    j <- numeric(1)
    for (i in i:(i+NIndex-1)) {
        j<-j+1
        smry.[j,]<-scan(file.,skip=i,nlines=1,nmax=7,quiet=TRUE,na.strings=na.strings)
    }

    i<-skip.hash(i)
    i<-skip.hash(i)
    index.<-array(0,dim=c(skip.until.minus.1(i)-i, 4))
    for (j in i:(skip.until.minus.1(i)-1))
        index.[j-i+1,]<-scan(file.,skip=j,nlines=1,nmax=4,quiet=T,na.strings=na.strings)
    i<-skip.until.minus.1(i)+1	   	
    p.<-read.table(file.,skip=i,fill=T,nrows=(skip.until.minus.1(i)-i-3),colClasses="numeric",na.strings=na.strings)
	  l. <- FLIndices()
    #for (i in 1:NIndex)
    #    l.[[i]]<-FLIndex(sel.pattern=FLQuant(NA,dimnames=list(age=range["min"]:range["max"],year=range["minyear"]:range["maxyear"])))
    
    return(set.index(smry.,index.,p.,l.,range))
}


set.index <- function(smry.,index.,p.,l.,range) {
    yr.range  <- tapply(index.[,2],index.[,1],range)
	for (i in 1:length(l.)) {
    	    l.[[i]]@range[1:2]<-smry.[i,6:7]
    	    l.[[i]]@range[4:5]<-yr.range[[i]]

    	    # TIMING (-1 = AVERAGE DURING YEAR, POSITIVE INTEGER = NUMBER OF MONTHS ELAPSED)
    	    if (smry.[i,5]==-1) 
    	        l.[[i]]@range[6:7]<-c(-1,-1)
    	    else   
    	        l.[[i]]@range[6:7]<-c(smry.[i,5]/12,smry.[i,5]/12)
    	    names(l.[[i]]@range)[6:7]<-c("startf","endf")

    	    # PDF         (0= do not use,1=lognormal, 2=normal)
    	    if (smry.[i,2]==2)
    	        l.[[i]]@distribution<-"normal"
    	    else 
    	        l.[[i]]@distribution<-"lognormal"

    	    # UNITS       (1 = numbers, 2 = biomass)
    	    if (smry.[i,3]==2) 
    	        l.[[i]]@type<-"biomass" 
    	    else 
    	        l.[[i]]@type<-"numbers"

    	    # SELECTIVITY (1 = fixed, 2 = fractional catches, 
    	    # 3 = Powers and Restrepo partial catches,4=Butterworth and Geromont eq 4)
            if (smry.[i,4]==1) 
                l.[[i]]@type<-c(l.[[i]]@type,"sel") 
            if (smry.[i,4]==2) 
                l.[[i]]@type<-c(l.[[i]]@type,"catches")
            if (smry.[i,4]==3) 
                l.[[i]]@type<-c(l.[[i]]@type,"Powers")
            if (smry.[i,4]==4) 
                l.[[i]]@type<-c(l.[[i]]@type,"Butterworth")  
            names(l.[[i]]@type)<-c("type")

            l.[[i]]@index <- FLQuant(array(index.[index.[,1]==i,3],
                                      dim     =c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                                      dimnames=list(age="all",year=(index.[index.[,1]==i,2]))))

			l.[[i]]@catch.wt <- FLQuant(l.[[i]]@index)
			l.[[i]]@catch.n  <- FLQuant(l.[[i]]@index)
			l.[[i]]@sel.pattern      <- FLQuant(l.[[i]]@index)

            l.[[i]]@effort <- FLQuant(array(1,
                dim=c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                dimnames=list(age="all",year=index.[index.[,1]==i,2])))

            l.[[i]]@index.var <- FLQuant(array(ifelse(index.[index.[,1]==i,4]<0,
                -1,index.[index.[,1]==i,4]), dim=c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                dimnames=list(age="all",year=index.[index.[,1]==i,2])))
            
           l.[[i]]@index.q <-FLQuant(1,dimnames=dimnames(l.[[i]]@index.q))
            
        if (any(p.[,1]==i)){
				l.[[i]]@sel.pattern <-FLQuant(array(t(as.matrix(p.[p.[,1]==i,3:length(p.[1,])])),
								  dim=c(length(p.[1,])-2,yr.range[[i]][2]-yr.range[[i]][1]+1),
                                  dimnames=list(as.character(range["min"]:range["max"]),year=index.[index.[,1]==i,2])))
				l.[[i]]@sel.pattern <-l.[[i]]@sel.pattern[as.character(smry.[i,6]:smry.[i,7]),,,,]
				}
			}
    return(l.)
}	# }}}

# read.FLIndex
read.FLIndex <- function(...)
{
  warning("read.FLIndex has been renamed as readFLIndex and will de deprecated", inmediate. = TRUE)
  readFLIndex(...)
}

# read.FLIndices
read.FLIndices <- function(...)
{
  warning("read.FLIndices has been renamed as readFLIndices and will de deprecated", inmediate. = TRUE)
  readFLIndices(...)
}
