# This script is based on CP Millar script's to read in lowestoft input files and plot them (6/10/2003)
# Modified by Marco Kienzle on 28 Nov. 2005

# PURPOSE: read ICA output file and retrieve the stock summary table

read.ica<-function(filename=""){

# check for accessablility
read.access <- file.access(filename, mode=4)
if (sum(read.access)!=0) {print(paste("File cannot be read : ",filename[read.access==-1]))}

# Extract the summary from each one

  if (read.access==0) {

    search <- "STOCK SUMMARY"            
data <- file(filename,open="r")
    count<-0; txt<-""
	

     while(!length(grep(search,txt)) && length(txt)) {

     count[1]<-count[1]+1
      txt  <-readLines(data,n=1);

    }
     search <- "----"
    count[2]<-0; txt<-""
     while(!length(grep(search,txt)) && length(txt)) {

      count[2]<-count[2]+1
      txt  <- readLines(data,n=1);
    }

    close(data)

    summary <- scan(filename,numeric(),skip=(count[1]+5),nlines=(count[2]-7),na.string=c("******","*******"))
#, allowEscapes=FALSE)

    summary <- matrix(summary,ncol=8,byrow=T)

      years <- summary[,1] # for dimnames
      summary <- summary[,2:7]

  }


# organise data
summary<-as.data.frame(summary)
dim(summary)

dimnames(summary)[[1]]<-years #seq(years[1],by=1,length=dim(summary)[1])
dimnames(summary)[[2]] <- c("Rec","TSB","SSB","Lands","Yield/SSB","FBar")

result<-list(filename=filename,data=summary)

return(result)}
