skip.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
        i <- i+1

    return(i)}

skip.until.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)!="#")
        i <- i+1

    return(i)} 

skip.until.minus.1<-function(i,x) {
        i<-i+1
        while (scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
            i<-i+1
        return(i)}



