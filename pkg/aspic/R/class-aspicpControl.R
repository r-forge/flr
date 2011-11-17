setGeneric("aspicpControl", function(object, ...){
    value  <-  standardGeneric("aspicpControl")
    return(value)})

validaspicpControl <- function(object){
  return(TRUE)
  
  # Everything is fine
	return(TRUE)}

setClass("aspicpControl",
	representation(
		bounds   ="array",
    conv     ="numeric",  
    nRestart ="numeric",                                                                       
    nSteps   ="numeric", 
    maxF     ="numeric",                                                                                            
    wt       ="numeric",
    rnd      ="numeric"),
	prototype=prototype(
		bounds  =array(NA,c(length(c(c("b0","k","msy"),paste("q",seq(1),sep=""))),5),dimnames=list(params=c(c("b0","k","msy"),paste("q",seq(1),sep="")),c("fit","min","start","max","lambda"))),
    conv    =c("simplex"=1.0000E-08,"restart"=3.0000E-08,"F"=1.0000E-04),
    nRestart=6,                                                                       
    nSteps  =0, 
    maxF    =8.0000,                                                                                            
    wt      =0,
    rnd     =6745260
  ),
	validity=validaspicpControl
  )

setMethod("aspicpControl", signature(object="missing"),
aspicpControl.<-function(object,...){

  res=new("aspicpControl")

  return(res)})

is.aspicpControl <- function(x)
  return(inherits(x, "aspicpControl"))

setMethod("aspicpControl", signature(object="character"),
aspicpControl.<-function(object,...){
  
    res   =new("aspicpControl")
    
    ## control
    aspicpC=function(file) {  
          inp=scan(file,sep="\n",what=character())
          ctrl=mlply(inp[5:21], function(x) {
             tmp=strsplit(x," ")
             tmp=unlist(tmp)[nchar(unlist(tmp))>0]})
        ctrl=llply(ctrl,as.numeric)
        ctrl=llply(ctrl,function(x) x[!is.na(x)])

        return(ctrl)}
    
  ctrl=suppressWarnings(aspicpC(object))
      
  #  [8] "7  ## Number of fisheries (data series)"                                                                                           
  n     =ctrl[[8]]  
  params=FLPar("b0"=NA,"k"=NA,"msy"=NA)
  parNms=c(c("b0","k","msy"),paste("q",seq(n),sep=""))
  res@bounds=array(NA,c(length(c(c("b0","k","msy"),paste("q",seq(n),sep=""))),5),dimnames=list(params=parNms,c("fit","min","start","max","lambda")))
  
  # [10] "1.00000  ## B1/K (starting guess, usually 0 to 1)"                                                                                 
  res@bounds["b0", "start"]=ctrl[[10]][1]
  # [11] "3.0000E+04  ## MSY (starting guess)"                                                                                               
  res@bounds["msy","start"]=ctrl[[11]]
  # [12] "2.6700E+05  ## K (carrying capacity) (starting guess)"                                                                             
  res@bounds["k", "start"]=ctrl[[12]]
  # [13] "2.1126E-06  6.0195E-06  9.7627E-06  1.4944E-04  2.9980E-06  4.2138E-04  8.3406E-04    ## q (starting guesses -- 1 per data series)"
  res@bounds[parNms[-(1:3)],"start"]=ctrl[[13]][1:n]

  # [14] "0  1  1  1  1  1  1  1  1  1    ## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)"                                                   
  res@bounds[,"fit"]=ctrl[[14]]

  # [15] "1.0000E+02  1.0000E+07  ## Min and max constraints -- MSY"                                                                         
  res@bounds["msy",c("min","max")]=ctrl[[15]]
  # [16] "1.0000E+04  2.0000E+07  ## Min and max constraints -- K"                                                                           
  res@bounds["k",  c("min","max")]=ctrl[[16]]
  
  res@bounds[parNms[-(1:3)],"min"]=res@bounds[parNms[-(1:3)],"start"]*0.01  
  res@bounds[parNms[-(1:3)],"max"]=res@bounds[parNms[-(1:3)],"start"]*100  
  res@bounds["b0","min"]=0.01  
  res@bounds["b0","max"]=1  
    
  #  [9] "1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E-02  1.0000E+00  1.0000E-02    ## Statistical weights for data series"      
  res@bounds[parNms[-(1:3)],"lambda"]=ctrl[[9]] 
 
  # [17] "6745260  ## Random number seed
  res@rnd   =ctrl[[17]]
    
  #  [3] "1.0000E-08  ## Convergence crit. for simplex"                                                                                      
  res@conv["simplex"]=ctrl[[3]]
    
  #  [4] "3.0000E-08  6  ## Convergence crit. for restarts, N restarts"                                                                      
  res@conv["restart"]=ctrl[[4]][1]
  res@nRestart=ctrl[[4]][1]
 
  #  [5] "1.0000E-04  0  ## Conv. crit. for F; N steps/yr for gen. model"                                                                    
  res@conv["F"]=ctrl[[5]][1]
  res@nSteps   =ctrl[[5]][1]
    
  #  [6] "8.0000  ## Maximum F when cond. on yield"                                                                                          
  res@maxF    =ctrl[[6]]
  #  [7] "0.0  ## Stat weight for B1>K as residual (usually 0 or 1)" 
  res@wt      =ctrl[[7]][1]
    
  return(res)})


writeaspicpControl=function(object,dir="/home/lkell/tmp"){
    file=paste(dir,"aspicp.inp",sep="/")
    comment=rep("",22)
    comment[ 1]= "\tFIT  ## Run type (FIT, BOT, or IRF)\n"                                                                                               
    comment[ 2]= "\tBase Case 01 (period 1959-2005)\n"                                                                                               
    comment[ 3]= "\tLOGISTIC  YLD    SSE\n"                                                                                                            
    comment[ 4]= "\t12 ## Verbosity\n"                                                                                                                  
    comment[ 5]= "\t## Number of bootstrap trials, <= 1000\n"                                                                                     
    comment[ 6]= "\t## 0=no MC search, 1=search, 2=repeated srch; N trials\n"                                                                  
    comment[ 7]= "\t## Convergence crit. for simplex\n"                                                                                      
    comment[ 8]= "\t## Convergence crit. for restarts, N restarts\n"                                                                      
    comment[ 9]= "\t## Conv. crit. for F; N steps/yr for gen. model\n"                                                                    
    comment[10]= "\t## Maximum F when cond. on yield\n"                                                                                          
    comment[11]= "\t## Stat weight for B1>K as residual (usually 0 or 1)\n"                                                                         
    comment[12]= "\t## Number of fisheries (data series)\n"                                                                                           
    comment[13]= "\t## Statistical weights for data series\n"      
    comment[14]= "\t## B1/K (starting guess, usually 0 to 1)\n"                                                                                 
    comment[15]= "\t## MSY (starting guess)\n"                                                                                               
    comment[16]= "\t## K (carrying capacity) (starting guess)\n"                                                                             
    comment[17]= "\t## q (starting guesses -- 1 per data series)\n"
    comment[18]= "\t## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)\n"                                                   
    comment[19]= "\t## Min and max constraints -- MSY\n"                                                                         
    comment[20]= "\t## Min and max constraints -- K\n"                                                                           
    comment[21]= "\t## Random number seed\n"                                                                                                    
    comment[22]= "\t## Number of years of data in each series\n" 

    cat(1,comment[ 1],         file=file,append=FALSE)
    cat(1,comment[ 2],         file=file,append=TRUE)
    cat(1,comment[ 3],         file=file,append=TRUE)
    cat(1,comment[ 4],         file=file,append=TRUE)
    cat(1,comment[ 5],         file=file,append=TRUE)
    cat(1,comment[ 6],         file=file,append=TRUE)
    cat(aspC@conv["simplex"],comment[ 7],         file=file,append=TRUE)
    cat(aspC@conv["restart"],comment[ 8],         file=file,append=TRUE)
    cat(aspC@conv["F"],      comment[ 9],         file=file,append=TRUE)
    cat(1,comment[10],         file=file,append=TRUE)
    cat(1,comment[11],         file=file,append=TRUE)
    cat(1,comment[12],         file=file,append=TRUE)
    cat(1,comment[13],         file=file,append=TRUE)
    cat(1,comment[14],         file=file,append=TRUE)
    cat(1,comment[15],         file=file,append=TRUE)
    cat(1,comment[16],         file=file,append=TRUE)
    cat(1,comment[17],         file=file,append=TRUE)
    cat(aspC@bounds[,"fit"],              comment[18],         file=file,append=TRUE)
    cat(aspC@bounds["msy",c("min","max")],comment[19],         file=file,append=TRUE)
    cat(aspC@bounds["msy",c("min","max")],comment[20],         file=file,append=TRUE)
    cat(object@rnd,                       comment[21],         file=file,append=TRUE)
    }
